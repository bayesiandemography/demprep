
#' Converting dates to cohorts
#' \code{open_left} is defaults to FALSE if \code{break_min}
#' not supplied, since count will always be 0.)
#'
#' if \code{break_min} supplied, then \code{origin} and
#' \code{month_start} both ignored.
#' 
#' @name date_to_cohort
NULL

## HAS_TESTS
#' @rdname date_to_cohort
#' @export
date_to_cohort_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE,
                                break_min = NULL,
                                open_left = NULL,
                                as_factor = TRUE) {
    if (is.null(open_left))
        open_left <- !is.null(break_min)
    date_to_period_or_cohort_year(date = date,
                                  month_start = month_start,
                                  label_year_start = label_year_start,
                                  break_min = break_min,
                                  open_left = open_left,
                                  as_factor = as_factor)
}

## HAS_TESTS
#' @rdname date_to_cohort
#' @export
date_to_cohort_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 month_start = "Jan",
                                 break_min = NULL,
                                 open_left = NULL,
                                 as_factor = TRUE) {
    if (is.null(open_left))
        open_left <- !is.null(break_min)
    date_to_period_or_cohort_multi(date = date,
                                   width = width,
                                   origin = origin,
                                   month_start = month_start,
                                   break_min = break_min,
                                   open_left = open_left,
                                   as_factor = as_factor)
}

## HAS_TESTS
#' @rdname date_to_cohort
#' @export
date_to_cohort_quarter <- function(date,
                                   break_min = NULL,
                                   open_left = NULL,
                                   as_factor = TRUE) {
    if (is.null(open_left))
        open_left <- !is.null(break_min)
    date_to_period_or_cohort_quarter(date = date,
                                     break_min = break_min,
                                     open_left = open_left,
                                     as_factor = as_factor)
}

## HAS_TESTS
#' @rdname date_to_cohort
#' @export
date_to_cohort_month <- function(date,
                                 break_min = NULL,
                                 open_left = NULL,
                                 as_factor = TRUE) {
    if (is.null(open_left))
        open_left <- !is.null(break_min)
    date_to_period_or_cohort_month(date = date,
                                   break_min = break_min,
                                   open_left = open_left,
                                   as_factor = as_factor)
}
