
#' Converting dates to cohorts
#'
#' @name date_to_cohort
NULL

## HAS_TESTS
#' @rdname date_to_cohort
#' @export
date_to_cohort_year <- function(date,
                                break_min = NULL,
                                break_max = NULL,
                                year_to = TRUE,
                                open_left = NULL,
                                as_factor = TRUE) {
    if (is.null(open_left))
        open_left <- !is.null(break_min)
    date_to_period_or_cohort_year(date = date,
                                  break_min = break_min,
                                  break_max = break_max,
                                  year_to = year_to,
                                  open_left = open_left,
                                  as_factor = as_factor)
}

#' @rdname date_to_cohort
#' @export
date_to_cohort_multi <- function(date,
                                 break_min = NULL,
                                 break_max = NULL,
                                 width = 5,
                                 open_left = NULL,
                                 as_factor = TRUE) {
    if (is.null(open_left))
        open_left <- !is.null(break_min)
    date_to_period_or_cohort_multi(date = date,
                                   break_min = break_min,
                                   break_max = break_max,
                                   width = width,
                                   open_left = open_left,
                                   as_factor = as_factor)
}

#' @rdname date_to_cohort
#' @export
date_to_cohort_quarter <- function(date,
                                   break_min = NULL,
                                   break_max = NULL,
                                   open_left = NULL,
                                   as_factor = TRUE) {
    date <- demcheck::err_tdy_date(x = date,
                                   name = "date")
}


#' @rdname date_to_cohort
#' @export
date_to_cohort_month <- function(date,
                                 break_min = NULL,
                                 break_max = NULL,
                                 open_left = NULL,
                                 as_factor = TRUE) {
    date <- demcheck::err_tdy_date(x = date,
                         name = "date")
}


