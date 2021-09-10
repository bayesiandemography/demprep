
## HAS_TESTS
#' Convert dates to one-year age groups
#'
#' Use dates of events and dates of birth
#' to create one-year age groups.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' argument with length 1 is recycled.
#'
#' @param date Dates of events.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class
#' \code{Date} using function \code{\link[base]{as.Date}}.
#' @param dob Dates of birth.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class
#' \code{Date} using function \code{\link[base]{as.Date}}.
#'
#' @return an integer vector with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso The output from \code{date_to_age_year}
#' is often processed further using function
#' \code{\link{format_age_year}}.
#'
#' Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_quarter}}
#' and \code{\link{date_to_age_month}}.
#'
#' Other functions for creating one-year
#' units from dates are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#'
#' @examples
#' date_to_age_year(date = c("2024-03-27",
#'                           "2022-11-09"),
#'                  dob = c("2001-03-21",
#'                          "2000-07-13"))
#'
#' ## replicate date of birth
#' date_to_age_year(date = c("2024-03-27",
#'                           "2022-11-09"),
#'                  dob = "2011-05-18")
#' @export 
date_to_age_year <- function(date, dob) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    age_years
}    


## HAS_TESTS
#' Convert dates to quarter (three-month) age groups
#'
#' Use dates of events and dates of birth to
## create one-quarter (three-month) age groups. 
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' @inheritParams date_to_age_year
#'
#' @return an integer vector with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso The output from \code{date_to_age_quarter}
#' is typically processed further using function
#' \code{\link{format_age_quarter}}.
#'
#' Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_year}}
#' and \code{\link{date_to_age_month}}.
#'
#' Other functions for creating one-quarter
#' intervals from dates are
#' \code{\link{date_to_period_quarter}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#'
#' @examples
#' date_to_age_quarter(date = c("2024-03-27",
#'                              "2022-11-09"),
#'                     dob = c("2001-03-21",
#'                             "2000-07-13"))
#'
#' ## replicate date of event
#' date_to_age_quarter(date = "2024-03-27",
#'                     dob = c("2001-03-21",
#'                             "2000-07-13"))
#' 
#' @export
date_to_age_quarter <- function(date, dob) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    age_quarters
}

## HAS_TESTS
#' Convert dates to one-month age groups
#'
#' Use dates of events and dates of birth to
#' create one-month age groups.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' @inheritParams date_to_age_year
#'
#' @return an integer vector with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso The output from \code{date_to_age_month}
#' is typically processed further using function
#' \code{\link{format_age_month}}.
#'
#' Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_year}} and
#' \code{\link{date_to_age_quarter}}.
#'
#' Other functions for creating one-month
#' intervals from dates are
#' \code{\link{date_to_period_month}},
#' \code{\link{date_to_cohort_month}},
#' and \code{\link{date_to_triangle_month}}.
#'
#' @examples
#' date_to_age_month(date = c("2024-03-27",
#'                            "2022-11-09"),
#'                   dob = c("2021-03-21",
#'                           "2020-07-13"))
#'
#' ## replicate date of birth
#' date_to_age_month(date = c("2024-03-27",
#'                            "2022-11-09"),
#'                   dob = "2020-07-13")
#' 
#' @export
date_to_age_month <- function(date, dob) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_months
}


