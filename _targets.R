library(targets)
library(nuts)
library(eurostat)
library(sf)
library(stars)
library(tidyverse)

source("lib.R")

list(
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
            code %in% c("demo_r_find3", "teicp250")
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
                select(var, geo = to_code, time = TIME_PERIOD, value = values)
            }
          )
        )
    }
  )
)
