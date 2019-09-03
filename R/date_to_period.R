

#' @rdname date_to_period
#' @export
date_to_period_year <- function(date,
                                first_month = "Jan",
                                year_to = TRUE,
                                as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
    first_month <- err_tdy_first_month(x = first_month,
                                       name = "first_month")
    err_is_logical_flag(x = year_to,
                        name = "year_to")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    breaks <- make_breaks_date_year(date = date,
                                    origin = 2000L,
                                    width = 1L,
                                    first_month = first_month)
    labels <- make_labels_period_year(breaks = breaks,
                                      open_left = FALSE,
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


make_breaks_date_year <- function(date, origin, width, first_month) {
    date_min <- min(date, na.rm = TRUE)
    date_max <- max(date, na.rm = TRUE)
    DATE_MIN <- as_ymd(date_min)
    DATE_MAX <- as_ymd(date_max)
    year_from  <- DATE_MIN$y
    year_to  <- DATE_MAX$y
    year_from <- origin - ((origin - year_from) %/% width + 1L) * width
    year_to <- origin + ((year_to - origin) %/% width + 1L) * width
    month_from_to <- match(first_month, month.abb)
    if (DATE_MIN$m < month_from_to)
        year_from <- year_from - width
    if (DATE_MAX$m >= month_from_to)
        year_to <- year_to + width
    date_from <- ISOdate(year = year_from,
                         month = month_from_to,
                         day = 1L)
    date_to <- ISOdate(year = year_to,
                       month = month_from_to,
                       day = 1L)
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
    date <- err_tdy_date(x = date,
                         name = "date")
    width <- err_tdy_positive_integer_scalar(x = width,
                                             name = "width")
    origin <- err_tdy_positive_integer_scalar(x = origin,
                                              name = "origin")
    first_month <- err_tdy_first_month(x = first_month,
                                       name = "first_month")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    breaks <- make_breaks_date_year(date = date,
                                    origin = origin,
                                    width = width,
                                    first_month = first_month)
    labels <- make_labels_period(breaks = breaks,
                                 open_left = FALSE,
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


#' @rdname date_to_period
#' @export
date_to_period_month <- function(date,
                                 as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
    err_is_logical_flag(as_factor)
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
    date <- err_tdy_date(x = date,
                         name = "date")
    err_is_logical_flag(as_factor)
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

    

