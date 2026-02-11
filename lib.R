set.seed(1337)

stable_columns <- c(
    "code",
    "freq",
    "unit",
    "geo",
    "TIME_PERIOD",
    "values",
    "OBS_FLAG",
    "CONF_STATUS",
    "OBS_VALUE"
)

#' Select columns sorted by name
select_sorted <- function(data) {
    data |> select(sort(names(data)))
}

#' Harmonise variable name e.g. to be used as python variable name
harmonise_var_name <- function(var) {
    var |>
        str_replace_all("[^A-z0-9]", "_") |>
        str_to_lower()
}

create_vars <- function(data, nuts_level, code) {
    data |>
        filter(nchar(geo) - 2 == nuts_level) |>
        select_sorted() |>
        unite(
            var,
            -any_of(stable_columns),
            sep = "_"
        ) |>
        mutate(var = map_chr(var, harmonise_var_name)) |>
        select(var, geo, freq, TIME_PERIOD, values) |>
        distinct(var, geo, TIME_PERIOD, .keep_all = TRUE)
}

resample_space_to_nuts3 <- function(data, nuts_codes) {
    # assume data has only one NUTS level
    nuts_level <- nchar(data$geo[[1]]) - 2

    if (nuts_level == 3) {
        # already at target resolution
        return(data)
    } else if (nuts_level == 2) {
        # up-sample NUTS2 to NUTS3 level
        res <- mutate(data, geo2 = geo)

        res <-
            nuts_codes |>
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
            nuts_codes |>
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
            mutate(
                TIME_PERIOD = paste0(
                    year(TIME_PERIOD),
                    "-Q",
                    quarter(TIME_PERIOD)
                )
            ) |>
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

get_var_meta <- function(cur_code, metabase) {
    # see docs https://ec.europa.eu/eurostat/web/user-guides/data-browser/api-data-access/api-detailed-guidelines/sdmx3-0/structure-queries
    doc <-
        str_glue(
            "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/structure/",
            "conceptscheme/ESTAT/{cur_code}?compress=false"
        ) |>
        read_xml()

    concepts <- list()
    names <- list()
    for (concept in xml_find_all(doc, "//s:Concept")) {
        id <- xml_attr(concept, "id")

        if (id %in% stable_columns) {
            next
        }
        name <- xml_find_first(concept, "c:Name[@xml:lang='en']") |> xml_text()
        values <- metabase |>
            filter(code == cur_code & name == id) |>
            pull(value)

        concepts[[id]] <- values
        names[[id]] <- name
    }
    names <- enframe(names) |> unnest(value)
    vars <- expand.grid(concepts) |> as_tibble()
    vars <- bind_cols(vars |> unite(col = "var"), vars) |>
        mutate(var = paste0(cur_code, "_", var))
    vars |>
        mutate(
            code = cur_code,
            label = apply(vars, 1, function(row) {
                paste(
                    paste(
                        names(vars)[names(vars) != "var"],
                        row[names(vars) != "var"],
                        sep = "="
                    ),
                    collapse = ","
                )
            })
        )
}
