
## HAS_TESTS
#' Convert dates to one-year age groups
#'
#' Given the dates when events occurred,
#' and the dates of birth of the people experiencing the events,
#' allocate the events to age groups.
#' All the resulting age groups have widths
#' of one year.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} years
#' old at their most recent birthday. For instance, a person
#' belongs to age group {"5"} if that person had their
#' 5th birthday two days ago, and a person belongs to age
#' group \code{"0"} if that person had their 0th birthday
#' (ie was born) three months ago.
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
#' Given the dates when events occurred,
#' and the dates of birth of the people experiencing the events,
#' allocate the events to age groups. The resulting
#' age groups all have widths of one quarter (ie three months).
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} quarters
#' old at their most recent birthday. For instance, a person
#' belongs to age group \code{"20q"} if that person had
#' their 5th birthday (= 20 quarters) two days ago.
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
#' Given the dates when events occurred,
#' and the dates of birth of the people experiencing the events,
#' allocate the events to age groups. These
#' age groups all have widths of one month.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} months
#' old at their most recent birthday.
#' For instance, a person belongs to age
#' group \code{"60m"} if that person had their
#' 5th birthday (= 60 months) two days ago.
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


