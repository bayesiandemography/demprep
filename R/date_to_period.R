
#' Converting dates to periods
#' \code{break_min} and \code{break_max} are for when 'date' does not
#' cover the entire range that is wanted
#'
#' @name date_to_period
NULL

## HAS_TESTS
#' @rdname date_to_period
#' @export
date_to_period_year <- function(date,
                                first_month = "Jan",
                                year_to = TRUE,
                                as_factor = TRUE) {
    date_to_period_or_cohort_year(date = date,
                                  first_month = first_month,
                                  year_to = year_to,
                                  break_min = NULL,
                                  open_left = NULL,
                                  as_factor = as_factor)
}

## HAS_TESTS
#' @rdname date_to_period
#' @export
date_to_period_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 first_month = "Jan",
                                 as_factor = TRUE) {
    date_to_period_or_cohort_multi(date = date,
                                   width = width,
                                   origin = origin,
                                   first_month = first_month,
                                   break_min = NULL,
                                   open_left = NULL,
                                   as_factor = as_factor)
}


#' @rdname date_to_period
#' @export
date_to_period_quarter <- function(date,
                                   as_factor = TRUE) {
    date <- demcheck::err_tdy_date(x = date,
                                   name = "date")
    demcheck::err_is_logical_flag(as_factor)
    breaks <- make_breaks_date_quarter(date)                                     
    labels <- make_labels_period_quarter(breaks = breaks,
                                         open_left = FALSE,
                                         open_right = FALSE)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans   
}


date_to_period_or_cohort_quarter <- function(date,
                                             as_factor) {
    date <- demcheck::err_tdy_date(x = date,
                                   name = "date")
    break_min <- demcheck::err_tdy_integer_scalar(x = break_min,
                                                  name = "break_min",
                                                  null_ok = TRUE)
    break_max <- demcheck::err_tdy_integer_scalar(x = break_max,
                                                  name = "break_max",
                                                  null_ok = TRUE)
    demcheck::err_is_ge_scalar(x1 = break_max,
                               x2 = break_min,
                               name1 = break_max,
                               name2 = break_min)
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    breaks <- make_breaks_date_quarter(date = date,
                                       break_min = break_min,
                                       break_max = break_max)
    include_na <- any(is.na(date))
    labels <- make_labels_period_year(breaks = breaks,
                                      open_left = open_left,
                                      open_right = FALSE,
                                      year_to = TRUE,
                                      include_na = include_na)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans   
}

make_breaks_date_quarter <- function(date,
                                     break_min,
                                     break_max) {
    if (is.infinite(break_min)) {
        date_first <- min(date, na.rm = TRUE)
        date_first_ymd <- as_ymd(date_first)
        year_first <- date_first_ymd$y
        month_first <- date_first_ymd$m
        year_from <- year_first
        month_from <- ((month_first - 1L) %/% 3L) * 3L + 1L
    }
    year_from <- break_min

    
    date_min <- min(date, na.rm = TRUE)
    date_max <- max(date, na.rm = TRUE)
    DATE_MIN <- as_ymd(date_min)
    DATE_MAX <- as_ymd(date_max)
    year_from  <- DATE_MIN$y
    year_to  <- DATE_MAX$y
    month_from <- DATE_MIN$m
    month_to <- (((DATE_MAX$m - 1L) %/% 3L) + 1L) * 3L + 1L
    if (month_to == 13L) {
        year_to <- year_to + 1L
        month_to <- 1L
    }
    date_from <- ISOdate(year = year_from,
                         month = month_from,
                         day = 1L)
    date_to <- ISOdate(year = year_to,
                       month = month_to,
                       day = 1L)
    seq.Date(from = date_from,
             to = date_to,
             by = "quarter")
}


#' @rdname date_to_period
#' @export
date_to_period_month <- function(date,
                                 as_factor = TRUE) {
    date <- demcheck::err_tdy_date(x = date,
                         name = "date")
    demcheck::err_is_logical_flag(as_factor)
    breaks <- make_breaks_date_month(date)                                     
    labels <- make_labels_period_month(breaks = breaks,
                                       open_left = FALSE,
                                       open_right = FALSE)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans   
}

make_breaks_date_month <- function(date) {
    date_min <- min(date, na.rm = TRUE)
    date_max <- max(date, na.rm = TRUE)
    DATE_MIN <- as_ymd(date_min)
    DATE_MAX <- as_ymd(date_max)
    year_from  <- DATE_MIN$y
    year_to  <- DATE_MAX$y
    month_from <- DATE_MIN$m
    month_to <- DATE_MAX$m + 1L
    if (month_to == 13L) {
        year_to <- year_to + 1L
        month_to <- 1L
    }
    date_from <- ISOdate(year = year_from,
                         month = month_from,
                         day = 1L)
    date_to <- ISOdate(year = year_to,
                       month = month_to,
                       day = 1L)
    seq.Date(from = date_from,
             to = date_to,
             by = "month")
}
