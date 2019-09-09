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
    demcheck::err_is_logical_flag(x = open_left,
                        name = "open_left")
    first_month <- demcheck::err_tdy_first_month(x = first_month,
                                       name = "first_month")
    demcheck::err_is_logical_flag(x = year_to,
                        name = "year_to")
    demcheck::err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    demcheck::err_is_gt_scalar(x1 = year_max,
                     x2 = year_min,
                     name1 = "year_max",
                     name2 = "year_min")
    demcheck::err_lt_year_min(date = date, ## make shortcut for -Inf
                    year_min = year_min,
                    first_month = first_month)
    demcheck::err_ge_year_max(date = date, ## make shortcut for Inf
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
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}



chk_ge_year_min <- function(date,
                            year_min,
                            first_month) {
    date_min <- sprintf("%d-%s-01", year_min, first_month)
    date_min <- as.Date(date_min, format = "%Y-%b-%d")
    lt_date_min <- !is.na(date) & (date < date_min)
    i <- match(TRUE, lt_date_min, nomatch = 0L)
    if (i > 0L) {
        return(gettextf(paste("'%s' has value [%s] that is less than the",
                              "minimum date implied  by '%s' and '%s' [%s]"),
                        "date",
                        date[[i]],
                        "year_min",
                        "first_month",
                        date_min))
    }
    TRUE
}


chk_lt_year_max <- function(date,
                            year_max,
                            first_month) {
    date_max <- sprintf("%d-%s-01", year_max, first_month)
    date_max <- as.Date(date_max, format = "%Y-%b-%d")
    ge_date_max <- !is.na(date) & (date >= date_max)
    i <- match(TRUE, ge_date_max, nomatch = 0L)
    if (i > 0L) {
        return(gettextf(paste("'%s' has value [%s] that is greater than or equal to the",
                              "maximum date implied  by '%s' and '%s' [%s]"),
                        "date",
                        date[[i]],
                        "year_max",
                        "first_month",
                        date_max))
    }
    TRUE
}

                        
        
    

make_breaks_date_year <- function(date,
                                  year_min,
                                  year_max,
                                  origin,
                                  width,
                                  first_month) {
    month_from_to <- match(first_month, month.abb)
    if (is.null(year_min)) {
        date_first <- min(date, na.rm = TRUE)
        date_first_ymd <- as_ymd(date_first)
        year_first <- date_first_ymd$y
        month_first <- date_first_ymd$m
        year_from <- origin - ((origin - year_first) %/% width + 1L) * width
        if (month_first < month_from_to)
            year_from <- year_from - width
    }
    else
        year_from <- year_min
    if (is.null(year_max)) {
        date_last <- max(date, na.rm = TRUE)
        date_last_ymd <- as_ymd(date_last)
        year_last <- date_last_ymd$y
        month_last <- date_last_ymd$m
        year_to <- origin - ((origin - year_last) %/% width + 1L) * width
        if (month_last >= month_from_to)
            year_to <- year_to + width
    }
    else
        year_to <- year_max
    date_from <- sprintf("%d-%d-%d", year_from, month_from_to, 1L)
    date_to <- sprintf("%d-%d-%d", year_to, month_from_to, 1L)
    date_from <- as.Date(date_from, format = "%Y-%m-%d")
    date_to <- as.Date(date_to, format = "%Y-%m-%d")
    by <- paste(width, "year")
    seq.Date(from = date_from,
             to = date_to,
             by = by)
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

    

