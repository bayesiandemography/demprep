
## HAS_TESTS (via date_to_cohort_period_year)
#' Convert dates to one-year periods
#'
#' Assign dates to one-year periods. The one-year periods are,
#' by default, calendar years.
#' 
#' Periods start on the first day of \code{month_start},
#' and end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' If a period starts on 1 January, then the first day and last day
#' of the period belong to the same calendar year.
#' But if a period starts on any other day, then
#' the first day belongs to one calendar
#' year and the last day belongs to the next calendar year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, then the first
#' day belongs to calendar year 2000, and the last
#' day belongs to calendar year 2001. Some people
#' use the first year to label such periods,
#' and others use the last year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, some people
#' label the period \code{"2000"}, and
#' others label it \code{"2001"}. Function
#' \code{date_to_period_year} by default uses
#' the start year. To use the end year, set
#' \code{label_year_start} to \code{FALSE}.
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
#' @return An integer vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_period_year}
#' is often processed further using \code{\link{format_period_year}}.
#'
#' Other functions for creating periods from dates are
#' \code{\link{date_to_period_quarter}} and
#' and \code{\link{date_to_period_month}}.
#'
#' Other functions for creating one-year units
#' from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#' The interface for \code{date_to_period_year} is identical
#' to that of \code{\link{date_to_cohort_year}}.
#'
#' @examples
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## period starts on 1 July, not 1 January
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul")
#'
#' ## period starts on 1 July, using the calendar year at
#' ## the end, rather than beginning, for the label
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#' @export
date_to_period_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE) {
    date_to_cohort_period_year(date = date,
                               month_start = month_start,
                               label_year_start = label_year_start)
}


## HAS_TESTS (via date_to_cohort_period_quarter)
#' Convert dates to quarter (three-month) periods
#'
#' Assign dates to one-quarter (three-month) periods
#' Quarters are defined as follows:
#' \tabular{lll}{
#'   \strong{Quarter} \tab \strong{Start} \tab \strong{End} \cr
#'   Q1 \tab 1 January \tab 31 March \cr
#'   Q2 \tab 1 April \tab 30 June \cr
#'   Q3 \tab 1 July \tab 30 September \cr
#'   Q4 \tab 1 October \tab 31 December
#' }
#'
#' @inheritParams date_to_period_year
#'
#' @return A character vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_period_quarter}
#' is often processed further using \code{\link{format_period_quarter}}.
#'
#' Other functions for creating periods from dates are
#' \code{\link{date_to_period_year}} and
#' and \code{\link{date_to_period_month}}.
#'
#' Other functions for creating one-quarter units
#' from dates are
#' \code{\link{date_to_age_quarter}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#' The interface for \code{date_to_period_quarter} is identical
#' to that of \code{\link{date_to_cohort_quarter}}.
#' 
#' @examples
#' date_to_period_quarter(date = c("2024-03-27",
#'                                 "2020-01-03",
#'                                 "2022-11-09"))
#' @export
date_to_period_quarter <- function(date) {
    date_to_cohort_period_quarter(date = date)
}


## HAS_TESTS (via date_to_cohort_period_month)
#' Convert dates to one-month periods
#'
#' Assign dates to one-month periods.
#'
#' @inheritParams date_to_period_year
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_period_month}
#' is often processed further using \code{\link{format_period_month}}.
#'
#' Other functions for creating periods from dates are
#' \code{\link{date_to_period_year}} and
#' and \code{\link{date_to_period_quarter}}.
#'
#' Other functions for creating one-month units
#' from dates are
#' \code{\link{date_to_age_month}},
#' \code{\link{date_to_cohort_month}},
#' and \code{\link{date_to_triangle_month}}.
#' The interface for \code{date_to_period_month} is identical
#' to that of \code{\link{date_to_cohort_month}}.
#'
#' @examples
#' date_to_period_month(date = c("2024-03-27",
#'                               "2020-01-03",
#'                               "2022-11-09"))
#' @export
date_to_period_month <- function(date) {
    date_to_cohort_period_month(date = date)
}

