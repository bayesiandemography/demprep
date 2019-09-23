
#' Converting dates to cohorts
#'
#' @name date_to_cohort
NULL

## HAS_TESTS
#' @rdname date_to_cohort
#' @export
date_to_cohort_year <- function(date,
                                first_month = "Jan",
                                year_to = TRUE,
                                break_min = NULL,
                                open_left = TRUE,
                                as_factor = TRUE) {
    date_to_period_or_cohort_year(date = date,
                                  first_month = first_month,
                                  year_to = year_to,
                                  break_min = break_min,
                                  open_left = open_left,
                                  as_factor = as_factor)
}

#' @rdname date_to_cohort
#' @export
date_to_cohort_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 first_month = "Jan",
                                 break_min = NULL,
                                 open_left = TRUE,
                                 as_factor = TRUE) {
    date_to_period_or_cohort_multi(date = date,
                                   width = width,
                                   origin = origin,
                                   first_month = first_month,
                                   break_min = break_min,
                                   open_left = open_left,
                                   as_factor = as_factor)
}

#' @rdname date_to_cohort
#' @export
date_to_cohort_quarter <- function(date,
                                   break_min = NULL,
                                   open_left = TRUE,
                                   as_factor = TRUE) {
    date <- demcheck::err_tdy_date(x = date,
                                   name = "date")
}


#' @rdname date_to_cohort
#' @export
date_to_cohort_month <- function(date,
                                 break_min = NULL,
                                 open_left = TRUE,
                                 as_factor = TRUE) {
    date <- demcheck::err_tdy_date(x = date,
                         name = "date")
}


