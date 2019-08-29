

date_to_period_year <- function(date,
                                first_month = "Jan",
                                is_year_to = TRUE,
                                as_factor = TRUE) {
}


date_to_period_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 first_month = "Jan",
                                 as_factor = TRUE) {
}



#' Convert dates to periods composed of single years
#' 
#' @export
date_to_period <- function(date,
                           width = 1,
                           origin = 2000,
                           first_month = "Jan",
                           is_year_to = TRUE,
                           as_factor = TRUE) {
    date <- err_tdy_date(x = date,
                         name = "date")
    if (all(is.na(date)))
        return(rep(NA_character_, times = length(date)))
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    breaks <- make_breaks_date(date = date,
                               width = width,
                               first_month = first_month)
    labels <- make_labels_period(breaks = breaks,
                                 open_left = FALSE,
                                 open_right = FALSE,
                                 is_year_to = is_year_to)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}


make_breaks_date <- function(date, width, first_month) {
    date_err_tdy_date(x = date,
                      name = "date")
    width <- err_tdy_positive_integer_scalar(x = width,
                                             name = "width")
    err_tdy_first_month(first_month)
    mday_first <- l$mday
    mon_first <- l$mon + 1L
    date_min <- min(date, na.rm = TRUE)
    date_max <- max(date, na.rm = TRUE)
    year_min <- as.integer(format(date_min, "%Y"))
    year_max <- as.integer(format(date_max, "%Y"))
    min_break <- as.Date(sprintf("%d-%d-%d",
                                 year_min,
                                 mon_first,
                                 mday_first))
    max_break <- as.Date(sprintf("%d-%d-%d",
                                 year_max,
                                 mon_first,
                                 mday_first))
    if (date_min < min_break)
        as.Date(sprintf("%d-%d-%d",
                        year_min - 1L,
                        mon_first,
                        mday_first))
    if (date_max >= max_break)
        as.Date(sprintf("%d-%d-%d",
                        year_max + 1L,
                        mon_first,
                        mday_first))
    by <- sprintf("%d year", width)
    seq.Date(from = min_break,
             to = max_break,
             by = by)
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
    




