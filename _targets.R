library(targets)
library(crew)
library(nuts)
library(eurostat)
library(sf)
library(stars)
library(RNetCDF)
library(tidyverse)

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
            code %in% selected_codes
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
            possibly(function(cur_code, data) {
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
                select(var, geo = to_code, time = TIME_PERIOD, value = values) |>
                resample_space_to_nuts3(nuts_codes)
            }, NA)
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
      dim.def.nc(nc, "time", length(times))
      dim.def.nc(nc, "geo", length(geos))

      # Variables
      var.def.nc(nc, "time", "NC_STRING", "time")
      var.def.nc(nc, "geo", "NC_STRING", "geo")

      var.put.nc(nc, "time", times)
      var.put.nc(nc, "geo", geos)

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
            select(-var) |>
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
          att.put.nc(grp, cur_var, "doi", "NC_CHAR", str_glue("https://doi.org/10.2908/{cur_code}"))
          var.put.nc(grp, cur_var, mat)
        }
      }

      close.nc(nc)
      nc_path
    }
  )
)
