library(targets)
library(crew)
library(nuts)
library(eurostat)
library(sf)
library(stars)
library(RNetCDF)
library(tidyverse)
library(xml2)

source("lib.R")
Sys.setenv(HDF5_USE_FILE_LOCKING = FALSE)

tar_option_set(
  controller = crew_controller_local(workers = 8),
  error = "continue"
)

list(
  tar_target(
    name = nuts_codes,
    command = {
      nuts::all_nuts_codes |>
        filter(version == "2024" & nchar(code) == 5) |>
        transmute(
          geo3 = code,
          geo2 = str_sub(code, 1, 4),
          geo1 = str_sub(code, 1, 3),
          geo0 = str_sub(code, 1, 2)
        ) |>
        arrange(geo3)
    }
  ),
  tar_target(
    name = nuts3_sf,
    command = {
      get_eurostat_geospatial(
        output_class = "sf",
        resolution = "20",
        nuts_level = "3",
        year = "2024"
      ) |>
        select(geo = id, name = NUTS_NAME, geometry) |>
        filter(geo %in% geos) |>
        arrange(geos)
    }
  ),
  # spatial data cube dimension
  tar_target(geos, nuts_codes$geo3),
  # temporal data cube dimension
  tar_target(
    name = times,
    command = {
      expand_grid(year = 2012:2025, quarter = c("Q1", "Q2", "Q3", "Q4")) |>
        unite(time, c(year, quarter), sep = "-") |>
        pull(time)
    }
  ),
  tar_target(
    name = metabase,
    command = {
      read_tsv(
        "https://ec.europa.eu/eurostat/api/dissemination/catalogue/metabase.txt.gz",
        col_names = c("code", "name", "value")
      )
    }
  ),
  tar_target(
    name = codes,
    command = c(
      "edat_lfse_04",
      "nama_10_nfa_bs"
      # "nama_10r_3gdp", "nama_10r_3gva", "nama_10r_2gvagr",
      # "teicp010", "teicp250", "nama_10_nfa_bs",
      # "lfst_r_lfu3pers", "demo_r_d3dens", "ilc_li02", "ilc_di11", "edat_lfse_04",
      # "nama_10r_2gfcf", "nama_10r_2emhrw"
    ),
  ),
  tar_target(
    name = variables_meta,
    pattern = map(raw_datasets),
    command = {
      raw_datasets |>
        mutate(
          data = map2(code, data, function(cur_code, data) {
            bind_cols(
              # unlabeled data
              data |>
                select(-c(geo, TIME_PERIOD, values)) |>
                distinct() |>
                select_sorted() |>
                select(freq, unit, everything()) |>
                unite("var", everything()) |>
                mutate(var = map_chr(var, harmonise_var_name)),
              # group labeled data
              data |>
                select(-c(geo, TIME_PERIOD, values)) |>
                distinct() |>
                select_sorted() |>
                select(-c(freq, unit)) |>
                unite("group", everything(), sep = " "),
              # labeled data
              data |>
                select(-c(geo, TIME_PERIOD, values)) |>
                distinct() |>
                select_sorted() |>
                label_eurostat(fix_duplicated = TRUE)
            ) |>
              mutate(code = cur_code) |>
              select(var, code, freq, unit, everything())
          })
        ) |>
        pull(data) |>
        reduce(full_join)
    }
  ),
  tar_target(
    name = variables_meta_file,
    format = "file",
    command = {
      path <- tar_path_target()
      write_csv(variables_meta, path)
      path
    }
  ),
  tar_target(
    name = datasets_meta,
    command = {
      get_eurostat_toc() |>
        mutate(
          last.update.of.data = as.Date(
            last.update.of.data,
            format = "%d.%m.%Y"
          )
        ) |>
        filter(
          type != "folder" &
            last.update.of.data > as.Date("2020-01-01") &
            values > 500 &
            code %in% codes
        ) |>
        # enforce code to be unique
        select(-hierarchy) |>
        distinct(code, .keep_all = TRUE)
    }
  ),
  tar_target(
    name = raw_datasets,
    pattern = map(datasets_meta),
    command = {
      datasets_meta |>
        transmute(
          code,
          data = map(code, ~ get_eurostat(.x, cache = FALSE))
        )
    }
  ),
  tar_target(
    name = classified_datasets,
    pattern = map(raw_datasets),
    command = {
      raw_datasets |>
        transmute(
          code,
          data = map2(
            data,
            code,
            function(data, code) {
              # discard aggregated data
              data <- filter(data, !str_starts(geo, "EU|EA|EEA"))

              # infer nuts_level by looking at NUTS codes
              nuts_level <- map_int(data$geo, nchar) |> max() - 2

              if (nuts_level == 0) {
                # no conversion needed at country level
                return(tibble())
              }

              temp_agg_data <-
                data |>
                create_vars(nuts_level, code) |>
                resample_time_to_quarter()

              nuts_classify(
                temp_agg_data,
                nuts_code = "geo",
                group_vars = intersect(
                  colnames(temp_agg_data),
                  c("var", "unit", "TIME_PERIOD")
                )
              )
            }
          )
        )
    }
  ),
  tar_target(
    name = datasets,
    pattern = map(classified_datasets),
    command = {
      classified_datasets |>
        transmute(
          code,
          data = map2(
            code,
            data,
            possibly(
              function(cur_code, data) {
                if (is_empty(data)) {
                  # no conversion needed at country level
                  res <-
                    raw_datasets |>
                    filter(code == cur_code) |>
                    pull(data) |>
                    first() |>
                    create_vars(nuts_level = 0, code = cur_code) |>
                    resample_time_to_quarter() |>
                    resample_space_to_nuts3(nuts_codes) |>
                    rename(value = values, time = TIME_PERIOD)
                  return(res)
                }

                data |>
                  nuts_convert_version(
                    to_version = "2024",
                    variables = c("values" = "absolute"),
                    weight = "pop21",
                    multiple_versions = "most_frequent"
                  ) |>
                  select(
                    var,
                    geo = to_code,
                    time = TIME_PERIOD,
                    value = values
                  ) |>
                  resample_space_to_nuts3(nuts_codes)
              },
              NA
            )
          )
        )
    }
  ),
  tar_target(
    name = cube,
    format = "file",
    command = {
      nc_path <- tar_path_target()
      nc <- create.nc(nc_path, format = "netcdf4")

      # Dimensions
      dim.def.nc(nc, "geo", length(geos))
      var.def.nc(nc, "geo", "NC_STRING", "geo")
      var.put.nc(nc, "geo", geos)

      dim.def.nc(nc, "time", length(times))
      var.def.nc(nc, "time", "NC_DOUBLE", "time")
      cf_times <- map_int(times, ~ yq(.x) - as.Date("1970-01-01"))
      var.put.nc(nc, "time", cf_times)
      att.put.nc(nc, "time", "units", "NC_CHAR", "days since 1970-1-1 0:0:0")
      att.put.nc(nc, "time", "long_name", "NC_CHAR", "time")

      # fill data
      for (cur_code in datasets$code) {
        data <-
          datasets |>
          filter(code == cur_code) |>
          pull(data) |>
          first()

        if (!"tbl" %in% class(data)) {
          next
        }

        grp <- grp.def.nc(nc, cur_code)

        vars <- unique(data$var)

        for (cur_var in vars) {
          mat <-
            data |>
            filter(
              var == cur_var,
              time %in% times,
              geo %in% geos
            ) |>
            select(geo, time, value) |>
            mutate(
              geo = geo |> factor(levels = geos),
              time = time |> factor(levels = times)
            ) |>
            complete(geo, time, fill = list(value = NA)) |>
            pivot_wider(names_from = geo, values_from = value) |>
            select(-time) |>
            as.matrix()

          fill_value <- 9.96921e36
          mat[is.na(mat)] <- fill_value

          var.def.nc(grp, cur_var, "NC_DOUBLE", c("time", "geo"))
          att.put.nc(grp, cur_var, "_FillValue", "NC_DOUBLE", fill_value)
          var.put.nc(grp, cur_var, mat)

          dim_attrs <-
            variables_meta |>
            filter(var == cur_var) |>
            pivot_longer(everything()) |>
            filter(!is.na(value) & !name %in% c("var", "code")) |>
            deframe()

          for (attr_name in names(dim_attrs)) {
            att.put.nc(grp, cur_var, attr_name, "NC_CHAR", dim_attrs[attr_name] |> as.character())
          }

          long_name <- paste0(
            datasets_meta |>
              filter(code == cur_code) |>
              pull(title) |>
              first() |>
              str_remove(" by.*$"),
            " in ",
            dim_attrs["group"]
          )
          att.put.nc(grp, cur_var, "long_name", "NC_CHAR", long_name)

          att.put.nc(
            grp,
            cur_var,
            "doi",
            "NC_CHAR",
            str_glue("https://doi.org/10.2908/{cur_code}")
          )
        }
      }

      close.nc(nc)
      nc_path
    }
  )
)
