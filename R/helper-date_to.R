
## HAS_TESTS
date_to_cohort_period_year <- function(date,
                                       month_start,
                                       label_year_start) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    no_non_na <- identical(sum(!is.na(date)), 0L)
    if (no_non_na)
        return(as.integer(date))
    breaks <- make_breaks_date_to_date_year(date = date,
                                            month_start = month_start)
    year <- as.integer(format(breaks, "%Y"))
    if (identical(month_start, "Jan") || label_year_start)
        year <- year[-length(year)]
    else
        year <- year[-1L]
    i <- findInterval(x = date,
                      vec = breaks)
    ans <- year[i]
    ans
}

date_to_cohort_period_quarter <- function(date) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    obs <- !is.na(date)
    date_obs <- date[obs]
    year_obs <- format(date_obs, "%Y")
    month_obs <- format(date_obs, "%m")
    month_obs <- as.integer(month_obs)
    quarter_obs <- ((month_obs - 1L) %/% 3L) + 1L
    ans <- rep(NA_integer_, times = length(date))
    ans[obs] <- sprintf("%s Q%d", year_obs, quarter_obs)
    ans
}


date_to_cohort_period_month <- function(date) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    obs <- !is.na(date)
    date_obs <- date[obs]
    ans <- rep(NA_integer_, times = length(date))
    ans[obs] <- format(date_obs, "%Y %b")
    ans
}




