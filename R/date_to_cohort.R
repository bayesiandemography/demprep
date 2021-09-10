
## HAS_TESTS (via date_to_cohort_period_year)
#' Convert dates to one-year cohorts
#'
#' Assign dates to one-year cohorts.
#'
#' Cohorts start on the first day of \code{month_start},
#' and end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so cohorts by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' If a cohort starts on 1 January, then the first day and last day
#' of the cohort belong to the same calendar year.
#' But if a cohort starts on any other day, then
#' the first day belongs to one calendar
#' year and the last day belongs to the next calendar year.
#' For instance, if a cohort extends from
#' 1 July 2000 to 30 June 2001, then the first
#' day belongs to calendar year 2000, and the last
#' day belongs to calendar year 2001. Some people
#' use the first calendar year to label such cohorts,
#' and others use the last calendar year.
#' For instance, if a cohort extends from
#' 1 July 2000 to 30 June 2001, some people
#' label the cohort \code{"2000"}, and
#' others label it \code{"2001"}. Function
#' \code{date_to_cohort_year} by default uses
#' the start year. To use the end year, set
#' \code{label_year_start} to \code{FALSE}.
#'
#' @param date Dates of events defining cohorts (typically births).
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Cohorts start on
#' the first day of this month.
#' @param label_year_start Logical. Whether to label a cohort
#' by the calendar year at the beginning of the cohort
#' or the calendar year at the end. Defaults to \code{TRUE}.
#'
#' @return An integer vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_cohort_year}
#' is ofen processed further using function
#' \code{\link{format_cohort_year}}.
#'
#' Other functions for creating
#' cohorts from dates are
#' \code{\link{date_to_cohort_quarter}} and
#' and \code{\link{date_to_cohort_month}}.
#'
#' Other functions for creating one-year
#' intervals from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_period_year}},
#' and \code{\link{date_to_triangle_year}}.
#' The interface for \code{date_to_cohort_year} is identical
#' to that of \code{\link{date_to_period_year}}.
#'
#' @examples
#' date_to_cohort_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## starts on 1 July rather than 1 January
#' date_to_cohort_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul")
#'
#' ## periods starts on 1 July, rather than 1 January, and uses
#' ## calendar year at end, rather than beginning, for the label
#' date_to_cohort_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#' @export
date_to_cohort_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE) {
    date_to_cohort_period_year(date = date,
                               month_start = month_start,
                               label_year_start = label_year_start)
}


## HAS_TESTS (via date_to_cohort_period_quarter)
#' Convert dates to quarter (three-month) cohorts
#'
#' Assign dates to one-quarter (three-month) cohorts.
#' Quarters are defined as follows:
#' \tabular{lll}{
#'   \strong{Quarter} \tab \strong{Start} \tab \strong{End} \cr
#'   Q1 \tab 1 January \tab 31 March \cr
#'   Q2 \tab 1 April \tab 30 June \cr
#'   Q3 \tab 1 July \tab 30 September \cr
#'   Q4 \tab 1 October \tab 31 December
#' }
#'
#' @inheritParams date_to_cohort_year
#'
#' @return A character vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_cohort_quarter}
#' is often processed further using \code{\link{format_cohort_quarter}}.
#'
#' Other functions for creating cohorts from dates are
#' \code{\link{date_to_cohort_quarter}} and
#' and \code{\link{date_to_cohort_month}}.
#'
#' Other functions for creating one-quarter units
#' from dates are
#' \code{\link{date_to_age_quarter}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#' The interface for \code{date_to_cohort_quarter} is identical
#' to that of \code{\link{date_to_period_quarter}}.
#' 
#' @examples
#' date_to_cohort_quarter(date = c("2024-03-27",
#'                                 "2022-11-09",
#'                                 "2023-05-11"))
#' @export
date_to_cohort_quarter <- function(date) {
    date_to_cohort_period_quarter(date = date)
}


## HAS_TESTS  (via date_to_cohort_period_month)
#' Convert dates to one-month cohorts
#'
#' Assign dates to one-month cohorts.
#'
#' @inheritParams date_to_cohort_year
#'
#' @return A character vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_cohort_month}
#' is often processed further using \code{\link{format_cohort_month}}.
#'
#' Other functions for creating cohorts from dates are
#' \code{\link{date_to_cohort_year}} and
#' and \code{\link{date_to_cohort_quarter}}.
#'
#' Other functions for creating one-month units
#' from dates are
#' \code{\link{date_to_age_month}},
#' \code{\link{date_to_period_month}},
#' and \code{\link{date_to_triangle_month}}.
#' The interface for \code{date_to_cohort_month} is identical
#' to that of \code{\link{date_to_period_month}}.
#' 
#' @examples
#' date_to_cohort_month(date = c("2024-03-27",
#'                               "2023-05-01",
#'                               "2022-11-09"))
#' @export
date_to_cohort_month <- function(date) {
    date_to_cohort_period_month(date = date)
}
