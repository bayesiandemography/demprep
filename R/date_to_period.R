
#' Converting dates to periods
#' \code{break_min} and \code{break_max} are for when 'date' does not
#' cover the entire range that is wanted
#'
#' @name date_to_period
NULL

## HAS_TESTS
#' @rdname date_to_period
#' @export
date_to_period_year <- function(date,
                                first_month = "Jan",
                                year_to = TRUE,
                                as_factor = TRUE) {
    date_to_period_or_cohort_year(date = date,
                                  first_month = first_month,
                                  year_to = year_to,
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
                                 first_month = "Jan",
                                 as_factor = TRUE) {
    date_to_period_or_cohort_multi(date = date,
                                   width = width,
                                   origin = origin,
                                   first_month = first_month,
                                   break_min = NULL,
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
