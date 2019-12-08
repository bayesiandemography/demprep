
## HAS_TESTS
#' Convert dates to one-year cohorts
#'
#' Allocate dates of events to cohorts with widths of one year.
#' The cohorts by default align with calendar years.
#' The events are typically, but not always, births.
#' 
#' The interface for \code{date_to_cohort_year} is similar
#' to that of \code{\link{date_to_period_year}}, except that
#' \code{date_to_cohort_year} also has arguments \code{break_min}
#' and \code{open_first}.
#' 
#' Supplying a date for \code{break_min} defines
#' the first cohort, and is similar to
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_group_year}}. If
#' \code{open_first} is \code{TRUE}, then \code{break_min}
#' forms the upper limit for the first cohort, and there is no lower
#' limit. If \code{open_first} is \code{FALSE}, then
#' \code{break_min} forms the lower limit for the first cohort.
#'
#' If a value for \code{break_min} is supplied, then
#' cohorts start on the day and month specified by \code{break_min};
#' otherwise they start on first day of \code{month_start}.
#' Cohorts end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
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
#' If \code{open_first} is \code{TRUE}, then
#' \code{label_year_start} must also be \code{TRUE}.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate cohorts,
#' including cohorts that not appear in the data.
#'
#' @param date Dates of events defining cohorts.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Cohorts start on
#' the first day of this month.
#' @param label_year_start Logical. Whether to label a cohort
#' by the calendar year at the beginning of the cohort
#' or the calendar year at the end. Not needed for cohorts
#' that start on 1 January.
#' @param break_min The start date of the first cohort,
#' or \code{NULL} (the default.)
#' If non-\code{NULL}, \code{break_min} can be a single value of
#' class \code{\link[base]{Date}}, or a value that
#' can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}};
#' in either case, it must be the first day of
#' a month.
#' @param open_first Whether the first cohort
#' has no lower limit. If \code{break_min} is non-\code{NULL}
#' and \code{label_year_start} is \code{TRUE}, then
#' then \code{open_first} defaults to \code{TRUE};
#' otherwise it defaults to \code{FALSE}.
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
#' ## Specify oldest cohort, with open_first
#' ## at default value of TRUE
#' date_to_cohort_year(date = c("2024-03-27",
#'                              "2019-08-22",
#'                              "2022-11-09"),
#'                     break_min = "2020-01-01")
#'
#' ## Specify oldest cohort, with open_first = FALSE
#' date_to_cohort_year(date = c("2024-03-27",
#'                              "2019-08-22",
#'                              "2022-11-09"),
#'                     break_min = "2015-07-01",
#'                     open_first = FALSE)
#'
#' ## return non-factor
#' date_to_cohort_year(date = c("2024-03-27", "2022-11-09"),
#'                     as_factor = FALSE)
#' @export
date_to_cohort_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE,
                                break_min = NULL,
                                open_first = NULL,
                                as_factor = TRUE) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    if (is.null(break_min)) {
        month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                     name = "month_start")
    }
    else {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
        month_start <- format(break_min, format = "%b")
    }
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    if (is.null(open_first))
        open_first <- isTRUE(label_year_start) && !is.null(break_min)
    else
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (open_first && !label_year_start)
        stop(gettextf("'%s' is %s but '%s' is %s",
                      "open_first", "TRUE", "label_year_start", "FALSE"))
    if (!is.null(break_min) && !open_first)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    breaks <- make_breaks_date_year(date = date,
                                    month_start = month_start,
                                    width = 1L,
                                    origin = NULL,
                                    break_min = break_min)
    labels <- make_labels_cohort(breaks = breaks,
                                 open_first = open_first,
                                 label_year_start = label_year_start,
                                 include_na = FALSE)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
    ans
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
#' and \code{open_first}.
#'
#' Supplying a date for \code{break_min} defines
#' the first cohort. It is similar
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_group_multi}}. If
#' \code{open_first} is \code{TRUE}, then \code{break_min}
#' forms the upper limit for the first cohort, and there is no lower
#' limit. If \code{open_first} is \code{FALSE}, then
#' \code{break_min} forms the lower limit for the first cohort.
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
#' ## Specify oldest cohort, with open_first
#' ## at default value of TRUE
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                      break_min = "2015-01-01")
#'
#' ## Specify oldest cohort, with open_first = FALSE
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                      break_min = "2015-07-01",
#'                      open_first = FALSE)
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
                                 open_first = NULL,
                                 as_factor = TRUE) {
    if (is.null(open_first))
        open_first <- !is.null(break_min)
    date_to_period_or_cohort_multi(date = date,
                                   width = width,
                                   origin = origin,
                                   month_start = month_start,
                                   break_min = break_min,
                                   open_first = open_first,
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
#' and \code{open_first}.
#'
#' \code{breaks} is
#' used to define the points where each cohort starts and finishes.
#' The dates in \code{breaks} must all be the first day of
#' the same month of the year. For instance,
#' \code{breaks} could consist of
#' the values \code{"2010-01-01"} and \code{"2017-01-01"},
#' but not \code{"2010-01-01"} and \code{"2017-01-02"},
#' or \code{"2010-01-01"} and \code{"2017-02-01"}.
#'
#' The definition of the first cohort depends on the
#' value of \code{open_first}. If \code{open_first} is
#' is \code{TRUE} (the default), then the first cohort consists of
#' all dates before the first break. Setting
#' \code{open_first} to \code{TRUE} is essentially
#' the same as having no upper limit on the oldest age group.
#' If \code{open_first} is \code{FALSE}, then the first cohort
#' is defined by the first two dates.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate cohorts,
#' including cohorts that not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#' @param breaks Dates defining starts and ends of cohorts.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param open_first Whether the first cohort
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
#'                       open_first = FALSE)
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
## WHAT IF 'date' GREATER THAN max(breaks)??
## what about breaks of length 0??
date_to_cohort_custom <- function(date,
                                  breaks,
                                  open_first = TRUE,
                                  as_factor = TRUE) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    breaks <- demcheck::err_tdy_breaks_date_cohort(breaks = breaks,
                                                   open_first = open_first)
    n <- length(breaks)
    if (n > 0L) {
        break_min <- breaks[[1L]]
        break_max <- breaks[[n]]
        if (!open_first)
            demcheck::err_ge_break_min_date(date = date,
                                            break_min = break_min)
        demcheck::err_lt_break_max_date(date = date,
                                        break_max = break_max)
    }
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    ## deal with "empty" case where 'breaks' has length 0
    if (n == 0L) {
        if (has_date) {
            stop(gettextf("'%s' has length %d",
                          "breaks", 0L))
        }
        else {
            ans <- as.character(date)
            if (as_factor)
                ans <- factor(ans)
            return(ans)
        }
    }
    ## deal with "empty" case where 'date'
    ## has length 0 or is all NA, and we
    ## aren't making factor levels
    if (!has_date && !as_factor) {
        ans <- as.character(date)
        return(ans)
    }
    ## make labels for breaks
    labels <- make_labels_cohort(breaks = breaks,
                                 open_first = open_first,
                                 label_year_start = NULL,
                                 include_na = FALSE)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    ## return result
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
    ans
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
#' and \code{open_first}.
#'
#' Supplying a date for \code{break_min} defines
#' the first cohort. It is similar
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_group_multi}}. If
#' \code{open_first} is \code{TRUE}, then \code{break_min}
#' forms the upper limit for the first cohort, and there is no lower
#' limit. If \code{open_first} is \code{FALSE}, then
#' \code{break_min} forms the lower limit for the first cohort.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate cohorts,
#' including cohorts that not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#' @param break_min The start date of the first cohort,
#' or \code{NULL} (the default.)
#' If non-\code{NULL}, \code{break_min} can be a single value of
#' class \code{\link[base]{Date}}, or a value that
#' can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}};
#' in either case, it must be the first day of
#' a quarter.
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
#' ## Specify oldest cohort, with open_first
#' ## at default value of TRUE
#' date_to_cohort_quarter(date = c("2024-03-27",
#'                                 "2019-08-22",
#'                                 "2022-11-09"),
#'                        break_min = "2020-01-01")
#'
#' ## Specify oldest cohort, with open_first = FALSE
#' date_to_cohort_quarter(date = c("2024-03-27",
#'                                 "2019-08-22",
#'                                 "2022-11-09"),
#'                     break_min = "2015-07-01",
#'                     open_first = FALSE)
#'
#' ## return non-factor
#' date_to_cohort_quarter(date = c("2024-03-27", "2022-11-09"),
#'                     as_factor = FALSE)
#' @export
date_to_cohort_quarter <- function(date,
                                   break_min = NULL,
                                   open_first = NULL,
                                   as_factor = TRUE) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    if (has_break_min) {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
    }
    if (has_open_first)
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    else
        open_first <- has_break_min
    if (!open_first && has_break_min)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    ## deal with "empty" case where 'date'
    ## has length 0 or is all NA, and we
    ## aren't making factor levels
    making_levels <- as_factor && has_break_min && open_first
    if (!has_date && !making_levels) {
        ans <- as.character(date)
        if (as_factor)
            ans <- factor(ans)
        return(ans)
    }
    ## create sequence of breaks
    breaks <- make_breaks_date_quarter(date = date,
                                       break_min = break_min)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_cohort_quarter(break_min = break_min,
                                         break_max = break_max,
                                         open_first = open_first,
                                         include_na = FALSE)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    ## return result
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
    ans   
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
#' and \code{open_first}.
#' 
#' Supplying a date for \code{break_min} defines
#' the first cohort. It is similar
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_group_multi}}. If
#' \code{open_first} is \code{TRUE}, then \code{break_min}
#' forms the upper limit for the first cohort, and there is no lower
#' limit. If \code{open_first} is \code{FALSE}, then
#' \code{break_min} forms the lower limit for the first cohort.
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
#' ## Specify oldest cohort, with open_first
#' ## at default value of TRUE
#' date_to_cohort_month(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                        break_min = "2020-01-01")
#'
#' ## Specify oldest cohort, with open_first = FALSE
#' date_to_cohort_month(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                     break_min = "2015-07-01",
#'                     open_first = FALSE)
#'
#' ## return non-factor
#' date_to_cohort_month(date = c("2024-03-27",
#'                               "2022-11-09"),
#'                     as_factor = FALSE)
#' @export
date_to_cohort_month <- function(date,
                                 break_min = NULL,
                                 open_first = NULL,
                                 as_factor = TRUE) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    if (has_break_min) {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
    }
    if (has_open_first)
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    else
        open_first <- has_break_min
    if (!open_first && has_break_min)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    ## deal with "empty" case where 'date'
    ## has length 0 or is all NA, and we
    ## aren't making factor levels
    making_levels <- as_factor && has_break_min && open_first
    if (!has_date && !making_levels) {
        ans <- as.character(date)
        if (as_factor)
            ans <- factor(ans)
        return(ans)
    }
    ## create sequence of breaks
    breaks <- make_breaks_date_month(date = date,
                                     break_min = break_min)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_cohort_month(break_min = break_min,
                                       break_max = break_max,
                                       open_first = open_first,
                                       include_na = FALSE)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    ## return result
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
    ans   
}
