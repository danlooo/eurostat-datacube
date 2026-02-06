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
        select(var, geo, freq, TIME_PERIOD, values) |>
        distinct(var, geo, TIME_PERIOD, .keep_all = TRUE)
}

resample_space_to_nuts3 <- function(data, nuts3_regions, eurostat_regions) {
    nuts_level <-
        data |>
        ungroup() |>
        distinct(geo) |>
        inner_join(eurostat_regions) |>
        count(nut_level) |>
        arrange(-n) |>
        pull(nut_level) |>
        first()

    if (nuts_level == 3) {
        # already at target resolution
        return(data)
    } else if (nuts_level == 2) {
        # up-sample NUTS2 to NUTS3 level
        res <- mutate(data, geo2 = geo)

        res <-
            nuts3_regions |>
            select(geo2, geo3) |>
            filter(geo2 %in% res$geo2) |>
            left_join(res, by = "geo2") |>
            mutate(geo = geo3) |>
            select(-geo2, -geo3)
        return(res)
    } else if (nuts_level == 0) {
        # up-sample country to NUTS3 level
        res <- mutate(data, geo0 = geo)

        res <-
            nuts3_regions |>
            select(geo0, geo3) |>
            filter(geo0 %in% res$geo0) |>
            left_join(res, by = "geo0") |>
            mutate(geo = geo3) |>
            select(-geo0, -geo3)
        return(res)
    }
    stop("nuts_level value not implemented")
}

resample_time_to_quarter <- function(data, agg_func = mean) {
    "freq" %in% colnames(data) || stop("column freq required")

    freq <- data$freq[[1]]

    if (freq == "Q") {
        # already at target resolution
        return(data)
    } else if (freq == "M") {
        # aggregate monthly to quarterly
        res <-
            data |>
            mutate(TIME_PERIOD = paste0(year(TIME_PERIOD), "-Q", quarter(TIME_PERIOD))) |>
            group_by(var, geo, TIME_PERIOD) |>
            summarise(values = agg_func(values)) |>
            ungroup()
        return(res)
    } else if (freq == "A") {
        # upscale annual to quarterly
        res <- mutate(data, year = year(TIME_PERIOD))
        res <-
            res |>
            expand(year, quarter = paste0("Q", 1:4)) |>
            left_join(res, by = "year") |>
            mutate(TIME_PERIOD = paste0(year, "-", quarter)) |>
            select(-year, -quarter)
        return(res)
    }
    stop("freq value not implemented.")
}
