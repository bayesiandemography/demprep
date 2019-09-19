#' \code{year_min} and \code{year_max} are for when 'date' does not
#' cover the entire range that is wanted

#' @rdname date_to_period
#' @export
date_to_period_year <- function(date,
                                year_min = -Inf,
                                year_max = Inf,
                                first_month = "Jan",
                                year_to = TRUE,
                                as_factor = TRUE) {
    date_to_period_or_cohort_year(date = date,
                                  year_min = year_min,
                                  year_max = year_max,
                                  open_left = FALSE,
                                  first_month = first_month,
                                  year_to = year_to,
                                  as_factor = as_factor)
}



date_to_period_or_cohort_year <- function(date,
                                          year_min,
                                          year_max,
                                          open_left,
                                          first_month,
                                          year_to,
                                          as_factor) {
    date <- demcheck::err_tdy_date(x = date,
                                   name = "date")
    year_min <- demcheck::err_tdy_integer_scalar(x = year_min,
                                                 name = "year_min")
    year_max <- demcheck::err_tdy_integer_scalar(x = year_min,
                                                 name = "year_max")
    demcheck::err_is_ge_scalar(x1 = year_max,
                               x2 = year_min,
                               name1 = year_max,
                               name2 = year_min)
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    first_month <- demcheck::err_tdy_first_month(x = first_month,
                                                 name = "first_month")
    demcheck::err_is_logical_flag(x = year_to,
                                  name = "year_to")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    demcheck::err_ge_year_min(date = date,
                              year_min = year_min,
                              first_month = first_month)
    demcheck::err_lt_year_max(date = date,
                              year_max = year_max,
                              first_month = first_month)
    breaks <- make_breaks_date_year(date = date,
                                    year_min = year_min,
                                    year_max = year_max,
                                    width = 1L,
                                    origin = 2000L,
                                    first_month = first_month)
    labels <- make_labels_period_year(breaks = breaks,
                                      open_left = open_left,
                                      open_right = FALSE,
                                      year_to = year_to)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}


    
    
date_to_period_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 first_month = "Jan",
                                 as_factor = TRUE) {
    date <- demcheck::err_tdy_date(x = date,
                         name = "date")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                             name = "width")
    origin <- demcheck::err_tdy_positive_integer_scalar(x = origin,
                                              name = "origin")
    first_month <- demcheck::err_tdy_first_month(x = first_month,
                                       name = "first_month")
    demcheck::err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    breaks <- make_breaks_date_year(date = date,
                                    origin = origin,
                                    width = width,
                                    first_month = first_month)
    labels <- make_labels_period_multi(breaks = breaks,
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


make_breaks_date_quarter <- function(date) {
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

    

