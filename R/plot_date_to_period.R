
## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_period_year'
#' 
#' Create plot illustrating how function
#' \code{\link{date_to_period_year}} works.
#' 
#' \code{plot_date_to_period_year} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @param date Dates of events or measurements.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Each period starts on
#' the first day of this month.
#' @param label_year_start Whether to label a period
#' by the calendar year at the beginning of the period
#' or the calendar year at the end. Not needed for periods
#' that start on 1 January. Defaults to \code{TRUE}.
#'
#' @examples
#' plot_date_to_period_year(date = c("2024-03-27",
#'                                   "2022-11-09"))
#'
#' ## July to June
#' plot_date_to_period_year(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          month_start = "Jul")
#'
#' ## July to June, using the calendar year at
#' ## the end for the label
#' plot_date_to_period_year(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          month_start = "Jul",
#'                          label_year_start = FALSE)
#' @keywords internal
#' @export
plot_date_to_period_year <- function(date,
                                     month_start = "Jan",
                                     label_year_start = TRUE) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_year(date = date,
                                            month_start = month_start)
    ## create labels
    labels <- make_labels_period(breaks = breaks,
                                 label_year_start = label_year_start,
                                 include_na = FALSE)
    ## make plot
    plot_date_to_period(date = date,
                        breaks = breaks,
                        labels = labels)
}


## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_period_quarter'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_period_quarter}} works.
#' 
#' \code{plot_date_to_period_quarter} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_period_year
#'
#' @examples
#' plot_date_to_period_quarter(date = c("2021-11-24",
#'                                      "2022-04-09"))
#' @keywords internal
#' @export
plot_date_to_period_quarter <- function(date) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_quarter(date = date,
                                               break_min = NULL,
                                               has_break_min_arg = FALSE)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_period_quarter(break_min = break_min,
                                         break_max = break_max,
                                         include_na = FALSE)
    ## make plot
    plot_date_to_period(date = date,
                        breaks = breaks,
                        labels = labels)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_period_month'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_period_month}} works.
#' 
#' \code{plot_date_to_period_month} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_period_year
#'
#' @examples
#' plot_date_to_period_month(date = c("2021-11-24",
#'                                    "2022-04-09"))
#' @keywords internal
#' @export
plot_date_to_period_month <- function(date) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_month(date = date,
                                             break_min = NULL,
                                             has_break_min_arg = FALSE)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_period_month(break_min = break_min,
                                       break_max = break_max,
                                       include_na = FALSE)
    ## make plot
    plot_date_to_period(date = date,
                        breaks = breaks,
                        labels = labels)
}
