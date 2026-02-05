library(targets)
library(nuts)
library(eurostat)
library(sf)
library(stars)
library(tidyverse)

stable_columns <- c("code", "freq", "unit", "geo", "TIME_PERIOD", "values")

create_vars <- function(data, nuts_level, code) {
  data |>
    filter(nchar(geo) - 2 == nuts_level) |>
    mutate(cur_code = code) |>
    select(cur_code, everything()) |>
    unite(
      var,
      -any_of(setdiff(stable_columns, "cur_code")),
      sep = "_"
    ) |>
    select(var, geo, TIME_PERIOD, values) |>
    distinct(var, geo, TIME_PERIOD, .keep_all = TRUE)
}

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
        ) %>%
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
          data = map(
            data,
            function(data) {
              # discard aggregated data
              data <- filter(data, !str_starts(geo, "EU|EA|EEA"))

              # infer nuts_level by looking at NUTS codes
              nuts_level <- map_int(data$geo, nchar) |> max() - 2

              if (nuts_level == 0) {
                # no conversion needed at country level
                return(tibble())
              }

              data <- create_vars(data, nuts_level, code)

              data |>
                nuts_classify(
                  nuts_code = "geo",
                  group_vars = intersect(
                    colnames(data),
                    c("var", "freq", "unit", "TIME_PERIOD")
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
