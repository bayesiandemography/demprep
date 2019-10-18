
## HAS_TESTS
#' Convert dates to one-year periods
#'
#' Given dates when events occurred or measurements were made,
#' derive one-year periods.
#'
#' By default, one-year periods start on 1 January and end on
#' 31 December. However, by supplying other values of
#' \code{month_start}, periods can start on the first day
#' of any other month. For instance, if \code{month_start}
#' is \code{"July"}, then the periods start on 1 July
#' and end on 31 June of the following year.
#'
#' \code{month_start} can be an element of \code{\link[base]{month.name}},
#' or an element of \code{\link[base]{month.abb}}.
#'
#' If a period starts on 1 January, the first day and final day
#' belong to the same calendar year. But if a period starts on,
#' for example, 1 July, then the first day belongs on one calendar
#' year and the final day belongs to the next calendear year.
#' Some organizations label such periods using the start year,
#' and some label them using the end year. If, for instance,
#' a period extends from 1 July 2000 to 30 June 2001, some
#' organizations will label the period \code{"2000"}, and
#' others will label it \code{"2001"}. Function \code{date_to_period_year}
#' uses the start year when \code{label_year_start} is \code{FALSE}
#' and the end year when \code{label_year_start} is argument
#' \code{
#'
#' instance,
#' some organizations label the period between 1 July 2000
#' and 30 Jun 2001 \code{"2000"} and others will label it
#' \code{"2001"}. Function \code{date_to_period} labels according
#' to the 
#' 
#' S
#' 
#' 
#' A person belongs to age group \code{"a"} if that
#' person was exactly \code{a} years
#' old at their most recent birthday. For instance, a person
#' who had their fifth birthday two days ago belongs to age
#' group \code{"5"} and a person who was born (had their zero-th
#' birthday) three months ago belongs to age group \code{"0"}.
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_max} and \code{open_right} are used to specify
#' the oldest age group.
#' When \code{break_max} is non-\code{NULL} and
#' \code{open_right} is \code{TRUE}, the oldest
#' age group is \code{[break_max, Inf)} years. When
#' \code{break_max} is non-\code{NULL} and 
#' \code{open_right} is \code{FALSE}, the oldest age
#' group is \code{[break_max-1, break_max)} years.
#' When \code{break_max} is \code{NULL}, the oldest age
#' group depends on the highest value in the data.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate age groups,
#' including age groups that not appear in the data.
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
#' \code{\link{date_to_age_group_multi}},
#' \code{\link{date_to_age_group_lifetab}},
#' \code{\link{date_to_age_group_fert}},
#' \code{\link{date_to_age_group_custom}},
#' \code{\link{date_to_age_group_quarter}},
#' and \code{\link{date_to_age_group_month}}.
#' Other functions for working with one-year intervals are
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#' See \code{\link{make_labels_period_year}} for the rules
#' on constructing labels for periods.
#'
#' @examples
#' date_to_age_group_year(date = c("2024-03-27", "2022-11-09"),
#'                        dob = c("2001-03-21", "2000-07-13"))
#'
#' ## replicate date of birth
#' date_to_age_group_year(date = c("2024-03-27", "2022-11-09"),
#'                        dob = "2011-05-18")
#'
#' ## return non-factor
#' date_to_age_group_year(date = c("2024-03-27", "2022-11-09"),
#'                        dob = "2011-05-18",
#'                        as_factor = FALSE)
#'
#' ## alternative specifications for oldest age group
#' date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01")
#' date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = 80)
#' date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = NULL)
#' date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = NULL,
#'                        open_right = FALSE)
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
