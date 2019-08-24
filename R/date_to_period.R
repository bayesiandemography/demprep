
#' Convert dates to periods
#' 
#' @rdname date_to_period
#' @export
NULL

#' @rdname date_to_period
#' @export
date_to_period <- function(date,
                           width = 1, origin = NULL,
                           first_day = "1 January", is_year_to = TRUE,
                           as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
    date <- asPOSIXlt(date)
    width <- err_tdy_integer_scalar(x = width,
                                    name = "width")
    width <- err_is_positive_scalar(x = width,
                                    name = "width")
    is_multi <- width > 1L
    if (is_multi)
        origin <- err_tdy_integer_scalar(origin)
    l <- err_tdy_first_day(first_day)
    mday_first <- l$mday
    mon_first <- l$mon + 1L
    err_is_logical_flag(x = is_year_to,
                        name = "is_year_to")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    ## calculations
    year <- date$year
    mon <- date$mon + 1L
    mday <- date$mday
    is_ge_first_day  <- (mon > mon_first) | ((mon == mon_first) & (mday >= mday_first))
    
    if (is_multi) {
        year_min <- min(year, na.rm = TRUE)
        year_max <- max(year, na.rm = TRUE)
        from <- (year_min %/% width) * width
        to <- ((year_min %/% width) + 1L) * width
        breaks <- seq.int(from = from,
                          to = to,
                          by = width)
        labels <- make

                          
    }
    else
        ans <- year + past_first + is_year_to - 1L
    ans <- as.character(ans)
    if (as_factor)
        
}

#' @rdname date_to_period
#' @export
date_to_period_month <- function(date,
                                 as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
    err_is_logical_flag(as_factor)
    date_to_label_month(date = date,
                        min = NULL,
                        max = NULL,
                        open_left = FALSE,
                        open_right = FALSE,
                        as_factor = as_factor)
}
                        
#' @rdname date_to_period
#' @export
date_to_period_quarter <- function(date,
                                   as_factor = TRUE) {
}
    




