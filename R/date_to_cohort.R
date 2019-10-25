
## HAS_TESTS
#' Convert dates to one-year cohorts
#'
#' Allocate dates of events to cohorts with widths of one year.
#' The cohorts by default align with calendar years.
#' The events are typically, but not always, births.
#' 
#' The interface for \code{date_to_cohort_year} is the similar
#' to that of \code{\link{date_to_period_year}}, except that
#' \code{date_to_cohort_year} also has arguments \code{break_min}
#' and \code{open_left}.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' \code{break_min} is a single value of
#' class \code{\link[base]{Date}}, or a value that
#' can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' \code{break_min} must be the first day of
#' a month, e.g \code{"2020-01-01"} or
#' \code{"2020-04-01"}.
#' 
#' Supplying a value for \code{break_min} is essentially equivalent
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_year}}, in that it is a way of
#' defining how the oldest ages are handled. If
#' \code{open_left} is \code{TRUE}, then the first cohort
#' is \code{(-Inf, break_min)}; otherwise the
#' first cohort is \code{[break_min, break_min - 1)}.
#'
#' If \code{break_min} is specified, then
#' cohorts start on the day and month supplied;
#' otherwise they start on first day of \code{month_start}.
#' Cohorts end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"January"},
#' so when \code{break_min} is not specified,
#' cohorts by default start on 1 January and
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
#' day belongs to the year 2000, and the last
#' day belongs to the year 2001. Some authors
#' use the first year to label such cohorts,
#' and others use the last year.
#' For instance, if a cohort extends from
#' 1 July 2000 to 30 June 2001, some authors
#' label the cohort \code{"2000"}, and
#' others label it \code{"2001"}. Function
#' \code{date_to_cohort_year} by default uses
#' the start year. To use the end year, set
#' \code{label_year_start} to \code{FALSE}.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate cohorts,
#' including cohorts that not appear in the data.
#'
#' @param date Dates of events defining cohorts.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The cohort starts on
#' the first day of this month.
#' @param label_year_start Logical. Whether to label a cohort
#' by the calendar year at the beginning of the cohort
#' or the calendar year at the end. Not needed for cohorts
#' that start on 1 January.
#' @param break_min An integer or \code{NULL}.
#' @param open_left Whether the first cohort
#' has no lower limit. If \code{break_min} is \code{NULL},
#' then \code{open_left} is ignored. If \code{break_min} is
#' non-\code{NULL}, then \code{open_left} defaults to
#' \code{TRUE}.
#' @param as_factor Whether the return value is a factor.
#' Defaults to \code{TRUE}.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_multi}},
#' \code{\link{date_to_cohort_custom}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_cohort_month}}.
#' Other functions for working with one-year intervals are
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_period_year}},
#' and \code{\link{date_to_triangle_year}}.
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' @examples
#' date_to_cohort_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## July to June
#' date_to_cohort_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul")
#'
#' ## July to June, using the calendar year at
#' ## the end for the label
#' date_to_cohort_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#'
#' ## Specify oldest cohort, with open_left
#' ## at default value of TRUE
#' date_to_cohort_year(date = c("2024-03-27",
#'                              "2019-08-22",
#'                              "2022-11-09"),
#'                     break_min = "2020-01-01")
#'
#' ## Specify oldest cohort, with open_left = FALSE
#' date_to_cohort_year(date = c("2024-03-27",
#'                              "2019-08-22",
#'                              "2022-11-09"),
#'                     break_min = "2015-07-01",
#'                     open_left = FALSE)
#'
#' ## return non-factor
#' date_to_cohort_year(date = c("2024-03-27", "2022-11-09"),
#'                     as_factor = FALSE)
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
#' Convert dates to multi-year cohorts
#'
#' Allocate dates of events to multi-year cohorts.
#' The cohorts all have the same
#' with, which by default is 5 years.
#' The events are typically, but not always, births.
#'
#' The interface for \code{date_to_cohort_multi} is the similar
#' to that of \code{\link{date_to_period_multi}}, except that
#' \code{date_to_cohort_multi} also has arguments \code{break_min}
#' and \code{open_left}.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' \code{break_min} is a single value of
#' class \code{\link[base]{Date}}, or a value that
#' can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' \code{break_min} must be the first day of
#' a month, e.g \code{"2020-01-01"} or
#' \code{"2020-04-01"}.
#' 
#' Supplying a value for \code{break_min} is essentially equivalent
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_year}}, in that it is a way of
#' defining how the oldest ages are handled. If
#' \code{open_left} is \code{TRUE}, then the first cohort
#' is \code{(-Inf, break_min)}; otherwise the
#' first cohort is \code{[break_min, break_min - width)}.
#'
#' If \code{break_min} is specified, then
#' cohorts start on the day and month supplied;
#' otherwise they start on first day of \code{month_start}.
#' Cohorts end \code{widths}-years-minus-one-day later.
#' The default value for \code{month_start} is \code{"January"},
#' so when \code{break_min} is not specified,
#' cohorts by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' The location of the periods can be shifted
#' by using different values for \code{origin}.

#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate cohorts,
#' including cohorts that not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#' @param width The length, in whole years, of the cohorts.
#' Defaults to 5.
#' @param origin An integer. Defaults to 2000. 
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_year}},
#' \code{\link{date_to_cohort_custom}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_cohort_month}}.
#' Other functions for working with multi-year intervals are
#' \code{\link{date_to_age_group_multi}},
#' \code{\link{date_to_period_multi}},
#' and \code{\link{date_to_triangle_multi}}.
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' @examples
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"))
#'
#' ## width is 10
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      width = 5)
#'
#' ## origin is 2001
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      origin = 2001)
#'
#' ## July to June
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      origin = 2001,
#'                      month_start = "Jul")
#'
#' ## Specify oldest cohort, with open_left
#' ## at default value of TRUE
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                      break_min = "2015-01-01")
#'
#' ## Specify oldest cohort, with open_left = FALSE
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                      break_min = "2015-07-01",
#'                      open_left = FALSE)
#'
#' ## return non-factor
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2022-11-09"),
#'                      as_factor = FALSE)
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
#' Convert dates to customized cohorts
#'
#' Allocate dates of events
#' to cohorts with varying widths, though all widths
#' are measured in whole years.
#' The events are typically, but not always, births.
#'
#' The interface for \code{date_to_cohort_custom} is the similar
#' to that of \code{\link{date_to_period_custom}}, except that
#' \code{date_to_cohort_custom} also has arguments \code{break_min}
#' and \code{open_left}.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' \code{breaks} is also vector of class \code{\link[base]{Date}},
#' or can be coerced to to one. \code{breaks} is
#' used to define the points where each cohort starts and finishes.
#' The dates in \code{breaks} must all be the first day of
#' the same month of the year. For instance,
#' \code{breaks} could consist of
#' the values \code{"2010-01-01"} and \code{"2017-01-01"},
#' but not \code{"2010-01-01"} and \code{"2017-01-02"},
#' or \code{"2010-01-01"} and \code{"2017-02-01"}.
#'
#' The definition of the first cohort depends on the
#' value of \code{open_left}. If \code{open_left} is
#' is \code{TRUE} (the default), then the first cohort consists of
#' all dates before the first break. Setting
#' \code{open_left} to \code{TRUE} is essentially
#' the same as having no upper limit on the oldest age group.
#' If \code{open_left} is \code{FALSE}, then the first cohort
#' is defined by the first two dates.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate cohorts,
#' including cohorts that not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#' @param breaks Dates defining starts and ends of cohorts.
#' @param open_left Whether the first cohort
#' has no lower limit. Defaults to \code{TRUE}.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_year}},
#' \code{\link{date_to_cohort_multi}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_cohort_month}}.
#' Other functions for working with customised intervals are
#' \code{\link{date_to_age_group_custom}},
#' and \code{\link{date_to_period_custom}}.
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' @examples
#' ## cohorts start on 1 January
#' date_to_cohort_custom(date = c("2024-03-27",
#'                                "2018-11-09",
#'                                "2020-05-13",
#'                                "2021-03-02"),
#'                       breaks = c("2020-01-01",
#'                                  "2021-01-01",
#'                                  "2026-01-01"))
#'
#' ## cohorts start on 1 March
#' date_to_cohort_custom(date = c("2024-03-27",
#'                                "2018-11-09",
#'                                "2020-05-13",
#'                                "2021-03-02"),
#'                       breaks = c("2020-03-01",
#'                                  "2021-03-01",
#'                                  "2026-03-01"))
#'
#' ## first cohort is closed
#' date_to_cohort_custom(date = c("2024-03-27",
#'                                "2018-11-09",
#'                                "2020-05-13",
#'                                "2021-03-02"),
#'                       breaks = c("2018-01-01",
#'                                  "2021-01-01",
#'                                  "2026-01-01"),
#'                       open_left = FALSE)
#'
#'
#' ## return non-factor
#' date_to_cohort_custom(date = c("2024-03-27",
#'                                "2018-11-09",
#'                                "2020-05-13",
#'                                "2021-03-02"),
#'                       breaks = c("2020-01-01",
#'                                  "2021-01-01",
#'                                  "2026-01-01"),
#'                       as_factor = FALSE)
#' @export
date_to_cohort_custom <- function(date,
                                  breaks,
                                  open_left = TRUE,
                                  as_factor = TRUE) {
    date_to_period_or_cohort_custom(date = date,
                                    breaks = breaks,
                                    open_left = open_left,
                                    as_factor = as_factor)
}

## HAS_TESTS
#' Convert dates to quarter cohorts
#'
#' Allocate dates of events
#' to cohorts of length one quarter, ie three months.
#' The events are typically, but not always, births.
#' Q1 (the first quarter) starts on 1 January
#' and ends on 31 March; Q2 starts on 1 April and ends on
#' 30 June; Q3 starts on 1 July and ends on 30 September;
#' Q4 starts on 1 October and ends on 31 December.
#'
#' The interface for \code{date_to_cohort_quarter} is the similar
#' to that of \code{\link{date_to_period_quarter}}, except that
#' \code{date_to_cohort_quarter} also has arguments \code{break_min}
#' and \code{open_left}.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' \code{break_min} is a single value of
#' class \code{\link[base]{Date}}, or a value that
#' can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' \code{break_min} must be the first day of
#' a quarter, e.g \code{"2020-01-01"} or
#' \code{"2020-04-01"}.
#' 
#' Supplying a value for \code{break_min} is essentially equivalent
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_quarter}}, in that it is a way of
#' defining how the oldest ages are handled. If
#' \code{open_left} is \code{TRUE}, then the first cohort
#' is \code{(-Inf, break_min)}; otherwise the
#' first cohort is \code{[break_min, break_min - 1)}.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate cohorts,
#' including cohorts that not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_year}},
#' \code{\link{date_to_cohort_multi}},
#' \code{\link{date_to_cohort_custom}},
#' and \code{\link{date_to_cohort_month}}.
#' Other functions for working with one-quarter intervals are
#' \code{\link{date_to_age_group_quarter}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#' See \code{\link{make_labels_period_quarter}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' @examples
#' date_to_cohort_quarter(date = c("2024-03-27",
#'                                 "2022-11-09"))
#'
#' ## Specify oldest cohort, with open_left
#' ## at default value of TRUE
#' date_to_cohort_quarter(date = c("2024-03-27",
#'                                 "2019-08-22",
#'                                 "2022-11-09"),
#'                        break_min = "2020-01-01")
#'
#' ## Specify oldest cohort, with open_left = FALSE
#' date_to_cohort_quarter(date = c("2024-03-27",
#'                                 "2019-08-22",
#'                                 "2022-11-09"),
#'                     break_min = "2015-07-01",
#'                     open_left = FALSE)
#'
#' ## return non-factor
#' date_to_cohort_quarter(date = c("2024-03-27", "2022-11-09"),
#'                     as_factor = FALSE)
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
#' Convert dates to month cohorts
#'
#' Allocate dates of events (typically, but not always, births)
#' to cohorts of length one month.
#'
#' The interface for \code{date_to_cohort_month} is the similar
#' to that of \code{\link{date_to_period_month}}, except that
#' \code{date_to_cohort_month} also has arguments \code{break_min}
#' and \code{open_left}.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' \code{break_min} is a single value of
#' class \code{\link[base]{Date}}, or a value that
#' can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' \code{break_min} must be the first day of
#' a month, e.g \code{"2020-01-01"} or
#' \code{"2020-04-01"}.
#' 
#' Supplying a value for \code{break_min} is essentially equivalent
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_month}}, in that it is a way of
#' defining how the oldest ages are handled. If
#' \code{open_left} is \code{TRUE}, then the first cohort
#' is \code{(-Inf, break_min)}; otherwise the
#' first cohort is \code{[break_min, break_min - 1)}.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate cohorts,
#' including cohorts that not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_year}},
#' \code{\link{date_to_cohort_multi}},
#' \code{\link{date_to_cohort_custom}},
#' and \code{\link{date_to_cohort_quarter}}.
#' Other functions for working with one-month intervals are
#' \code{\link{date_to_age_group_month}},
#' \code{\link{date_to_period_month}},
#' and \code{\link{date_to_triangle_month}}.
#' See \code{\link{make_labels_period_month}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' @examples
#' date_to_cohort_month(date = c("2024-03-27",
#'                               "2022-11-09"))
#'
#' ## Specify oldest cohort, with open_left
#' ## at default value of TRUE
#' date_to_cohort_month(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                        break_min = "2020-01-01")
#'
#' ## Specify oldest cohort, with open_left = FALSE
#' date_to_cohort_month(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                     break_min = "2015-07-01",
#'                     open_left = FALSE)
#'
#' ## return non-factor
#' date_to_cohort_month(date = c("2024-03-27",
#'                               "2022-11-09"),
#'                     as_factor = FALSE)
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
