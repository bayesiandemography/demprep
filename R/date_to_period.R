
## HAS_TESTS
#' Convert dates to one-year periods
#'
#' Allocate dates to one-year periods. The periods are,
#' by default, calendar years.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' One-year periods start on the first day of \code{month_start},
#' and end one year minus one day later.
#' The default value for \code{"month_start"} is \code{"January"},
#' so one-year periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be an
#' a full or abbreviated month name: more precisely,
#' it can be an element of \code{\link[base]{month.name}},
#' or an element of \code{\link[base]{month.abb}}.
#'
#' If a period starts on 1 January, the first day and last day
#' of the period belong to the same calendar year.
#' But if a period starts on,
#' for example, 1 July, then the first day belongs to one calendar
#' year and the last day belongs to the next calendear year.
#' Some people use the start year to label periods that
#' cross calendar years, and others use the end year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, some people
#' label the period \code{"2000"}, and
#' others label it \code{"2001"}. Function
#' \code{date_to_period_year} by default uses
#' the start year. To use the end year, set
#' \code{label_year_start} to \code{FALSE}.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate periods,
#' including periods that not appear in the data.
#'
#' @param date Dates of events or measurements.
#' @param month_start
#' @param label_year_start
#' @param as_factor Whether the return value is a factor.
#' Defaults to \code{TRUE}.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The length of the return value equals the length
#' of \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_custom}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_period_month}}.
#' Other functions for working with one-year intervals are
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#' See \code{\link{make_labels_period_year}} for the rules
#' on constructing labels for periods.
#'
#' @examples
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## July to June
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul")
#'
#' ## July to Jun, using calendar year at end for label
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#'
#' ## return non-factor
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     as_factor = FALSE)
#' @export
date_to_period_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE,
                                as_factor = TRUE) {
    date_to_period_or_cohort_year(date = date,
                                  month_start = month_start,
                                  label_year_start = label_year_start,
                                  break_min = NULL,
                                  open_left = FALSE,
                                  as_factor = as_factor)
}

## HAS_TESTS
#' @rdname date_to_period
#' @export
date_to_period_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 month_start = "Jan",
                                 as_factor = TRUE) {
    date_to_period_or_cohort_multi(date = date,
                                   width = width,
                                   origin = origin,
                                   month_start = month_start,
                                   break_min = NULL,
                                   open_left = FALSE,
                                   as_factor = as_factor)
}

## HAS_TESTS
#' @rdname date_to_period
#' @export
date_to_period_custom <- function(date,
                                  breaks,
                                  as_factor = TRUE) {
    date_to_period_or_cohort_custom(date = date,
                                    breaks = breaks,
                                    open_left = FALSE,
                                    as_factor = as_factor)
}

## HAS_TESTS
#' @rdname date_to_period
#' @export
date_to_period_quarter <- function(date,
                                   as_factor = TRUE) {
    date_to_period_or_cohort_quarter(date = date,
                                     break_min = NULL,
                                     open_left = FALSE,
                                     as_factor = as_factor)
}

## HAS_TESTS
#' @rdname date_to_period
#' @export
date_to_period_month <- function(date,
                                 as_factor = TRUE) {
    date_to_period_or_cohort_month(date = date,
                                   break_min = NULL,
                                   open_left = FALSE,
                                   as_factor = as_factor)
}
