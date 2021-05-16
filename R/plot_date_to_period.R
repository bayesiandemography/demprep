
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
#' A vector of class \code{\link[base]{Date}},
#' or a vector than can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Each period starts on
#' the first day of this month.
#' @param label_year_start Whether to label a period
#' by the calendar year at the beginning of the period
#' or the calendar year at the end. Defaults to \code{TRUE}.
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
    plot_date_to_cohort_period_year(date = date,
                                    month_start = month_start,
                                    label_year_start = label_year_start)
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
    plot_date_to_cohort_period_quarter(date = date)
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
    plot_date_to_cohort_period_month(date = date)
}
