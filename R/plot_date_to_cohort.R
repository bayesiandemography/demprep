
## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_cohort_year'
#' 
#' Create a plot illustrating how function
#' \code{\link{date_to_cohort_year}} works.
#' 
#' \code{plot_date_to_cohort_year} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @param date Dates of events or measurements.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The cohort starts on
#' the first day of this month.
#' @param label_year_start Whether to label a cohort
#' by the calendar year at the beginning of the cohort
#' or the calendar year at the end. Not needed for cohorts
#' that start on 1 January. Defaults to \code{TRUE}.
#'
#' @examples
#' plot_date_to_cohort_year(date = c("2024-03-27",
#'                                   "2022-11-09"))
#'
#' ## starts on 1 July rather than 1 January
#' plot_date_to_cohort_year(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          month_start = "Jul")
#'
#' ## starts on 1 July rather than 1 January,
#' ## and uses calendar year at end rather than
#' ## calendar year at the beginning
#' plot_date_to_cohort_year(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          month_start = "Jul",
#'                          label_year_start = FALSE)
#' @keywords internal
#' @export
plot_date_to_cohort_year <- function(date,
                                     month_start = "Jan",
                                     label_year_start = TRUE) {
    plot_date_to_cohort_period_year(date = date,
                                    month_start = month_start,
                                    label_year_start = label_year_start)
}


## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_cohort_quarter'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_cohort_quarter}} works.
#' \code{plot_date_to_cohort_quarter} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_cohort_year
#'
#' @examples
#' plot_date_to_cohort_quarter(date = c("2024-03-27",
#'                                      "2022-05-13",
#'                                      "2022-11-09"))
#' @keywords internal
#' @export
plot_date_to_cohort_quarter <- function(date) {
    plot_date_to_cohort_period_quarter(date = date)
}


## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_cohort_month'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_cohort_month}} works.
#' 
#' \code{plot_date_to_cohort_month} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_cohort_year
#'
#' @examples
#' plot_date_to_cohort_month(date = c("2024-03-27",
#'                                    "2023-08-13",
#'                                    "2023-11-09"))
#' @keywords internal
#' @export
plot_date_to_cohort_month <- function(date) {
    plot_date_to_cohort_period_month(date = date)
}
