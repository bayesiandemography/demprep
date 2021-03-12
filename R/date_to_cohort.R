

## Note - all functions return empty factor when supplied with
## empty inputs, except for 'date_to_cohort_custom', which
## returns factor with levels implied by 'breaks' and
## 'open_first' (reflecting the fact that the levels are
## completely determined by these two arguments).


## HAS_TESTS
#' Convert dates to one-year cohorts
#'
#' Identify cohorts, based on dates of births or other events.
#' The cohorts have widths of one year.
#' 
#' The interface for \code{date_to_cohort_year} is similar
#' to that of \code{\link{date_to_period_year}}, except that
#' \code{date_to_cohort_year} also has arguments \code{break_min}
#' and \code{open_first}.
#' 
#' Supplying a date for \code{break_min} defines
#' the first cohort, and is similar to
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_year}}. If
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
#' If \code{open_first} can only be \code{TRUE},
#' if a value for \code{break_min} is supplied,
#' and if \code{label_year_start} is \code{TRUE}.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate cohorts,
#' including cohorts that do not appear in the data.
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
#' that start on 1 January. Defaults to \code{TRUE}.
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
#' and \code{label_year_start} is \code{TRUE},
#' then \code{open_first} defaults to \code{TRUE};
#' otherwise it defaults to \code{FALSE}.
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_multi}},
#' \code{\link{date_to_cohort_custom}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_cohort_month}}.
#'
#' Other functions for working with one-year intervals are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_period_year}},
#' and \code{\link{date_to_triangle_year}}.
#'
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' \code{\link{plot_date_to_cohort_year}} creates plots
#' that illustrate how \code{date_to_cohort_year}
#' works.
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
#' @export
date_to_cohort_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE,
                                break_min = NULL,
                                open_first = NULL) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    if (has_break_min) {
        demcheck::err_length_1(x = break_min,
                               name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
        month_start <- format(break_min, format = "%b")
    }
    else {
        month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                     name = "month_start")
    }
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
        if (open_first && !has_break_min)
            stop(gettextf("'%s' is %s but '%s' is %s",
                          "open_first", "TRUE", "break_min", "NULL"))
        if (open_first && !isTRUE(label_year_start))
            stop(gettextf("'%s' is %s but '%s' is %s",
                          "open_first", "TRUE", "label_year_start", "FALSE"))
    }
    else        
        open_first <- has_break_min && isTRUE(label_year_start)
    if (!open_first && has_break_min)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    ## deal with "empty" case where 'date'
    ## has length 0 or is all NA
    if (!has_date) {
        if (length(date) > 0L)
            ans <- factor(date,
                          levels = NA_character_,
                          exclude = NULL)
        else
            ans <- factor()
        return(ans)
    }
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_year(date = date,
                                    month_start = month_start,
                                    width = 1L,
                                    origin = NULL,
                                    break_min = break_min)
    ## make labels for these breaks
    include_na <- anyNA(date)
    labels <- make_labels_cohort(breaks = breaks,
                                 open_first = open_first,
                                 label_year_start = label_year_start,
                                 include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to multi-year cohorts
#'
#' Identify cohorts, based on dates of births or other events.
#' The cohorts all have the same width, which by default is 5 years.
#'
#' The interface for \code{date_to_cohort_multi} is the similar
#' to that of \code{\link{date_to_period_multi}}, except that
#' \code{date_to_cohort_multi} also has arguments \code{break_min}
#' and \code{open_first}.
#'
#' Supplying a date for \code{break_min} defines
#' the first cohort. It is similar
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_multi}}. If
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
#' by using different values for \code{origin},
#' unless a value for \code{break_min} has been
#' supplied, in which case \code{origin} is ignored.
#' 
#' If \code{open_first} can only be \code{TRUE},
#' if a value for \code{break_min} is supplied.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate cohorts,
#' including cohorts that do not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#' @param width The length, in whole years, of the cohorts.
#' Defaults to 5.
#' @param origin An integer. Defaults to 2000.
#' Ignored if value supplied for \code{break_min}.
#' @param open_first Whether the first cohort
#' has no lower limit. If \code{break_min} is non-\code{NULL},
#' then \code{open_first} defaults to \code{TRUE};
#' otherwise it defaults to \code{FALSE}.
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_year}},
#' \code{\link{date_to_cohort_custom}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_cohort_month}}.
#' 
#' Other functions for working with multi-year intervals are
#' \code{\link{date_to_age_multi}},
#' \code{\link{date_to_period_multi}},
#' and \code{\link{date_to_triangle_multi}}.
#'
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' \code{\link{plot_date_to_cohort_multi}} creates plots
#' that illustrate how \code{date_to_cohort_multi}
#' works.
#'
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
#' ## starts on 1 July rather than 1 January
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      origin = 2001,
#'                      month_start = "Jul")
#'
#' ## specify oldest cohort, with 'open_first'
#' ## at default value of 'TRUE'
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                      break_min = "2015-01-01")
#'
#' ## specify oldest cohort, with 'open_first'
#' ## set to 'FALSE'
#' date_to_cohort_multi(date = c("2024-03-27",
#'                               "2019-08-22",
#'                               "2022-11-09"),
#'                      break_min = "2015-07-01",
#'                      open_first = FALSE)
#' @export
date_to_cohort_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 month_start = "Jan",
                                 break_min = NULL,
                                 open_first = NULL) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if (has_break_min) {
        demcheck::err_length_1(x = break_min,
                               name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
        origin <- NULL
        month_start <- format(break_min, format = "%b")
    }
    else {
        origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                                   name = "origin")
        month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                     name = "month_start")
    }
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
        if (open_first && !has_break_min)
            stop(gettextf("'%s' is %s but '%s' is %s",
                          "open_first", "TRUE", "break_min", "NULL"))
    }
    else
        open_first <- has_break_min
    if (!open_first && has_break_min)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    ## deal with "empty" case where 'date'
    ## has length 0 or is all NA
    if (!has_date) {
        if (length(date) > 0L)
            ans <- factor(date,
                          levels = NA_character_,
                          exclude = NULL)
        else
            ans <- factor()
        return(ans)
    }
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_year(date = date,
                                    month_start = month_start,
                                    width = width,
                                    origin = origin,
                                    break_min = break_min)
    ## make labels for these breaks
    include_na <- anyNA(date)
    labels <- make_labels_cohort(breaks = breaks,
                                 open_first = open_first,
                                 label_year_start = NULL,
                                 include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Convert dates to customized cohorts
#'
#' Identify cohorts, based on dates of births or other events.
#' The cohorts have varying widths. All widths
#' are measured in whole years.
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
#' The return value is a factor. The levels of this
#' factor contain all intermediate cohorts,
#' including cohorts that do not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#' @param breaks Dates defining starts and ends of cohorts.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param open_first Whether the first cohort
#' has no lower limit. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_year}},
#' \code{\link{date_to_cohort_multi}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_cohort_month}}.
#'
#' Other functions for working with customised intervals are
#' \code{\link{date_to_age_custom}},
#' and \code{\link{date_to_period_custom}}.
#'
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' \code{\link{plot_date_to_cohort_custom}} creates plots
#' that illustrate how \code{date_to_cohort_custom}
#' works.
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
#' @export
date_to_cohort_custom <- function(date,
                                  breaks,
                                  open_first = TRUE) {
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
    n_break <- length(breaks)
    if (n_break > 0L) {
        break_min <- breaks[[1L]]
        break_max <- breaks[[n_break]]
        if (!open_first)
            demcheck::err_ge_break_min_date(date = date,
                                            break_min = break_min)
        demcheck::err_lt_break_max_date(date = date,
                                        break_max = break_max)
    }
    ## deal with "empty" case where 'breaks' has length 0
    if (n_break == 0L) {
        if (has_date) {
            stop(gettextf("'%s' has length %d",
                          "breaks", 0L))
        }
        else {
            if (length(date) > 0L)
                ans <- factor(date,
                              levels = NA_character_,
                              exclude = NULL)
            else
                ans <- factor()
            return(ans)
        }
    }
    ## make labels for breaks
    include_na <- anyNA(date)
    labels <- make_labels_cohort(breaks = breaks,
                                 open_first = open_first,
                                 label_year_start = NULL,
                                 include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to one-quarter cohorts
#'
#' Identify cohorts, based on dates of births or other events.
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
#' \code{\link{date_to_age_multi}}. If
#' \code{open_first} is \code{TRUE}, then \code{break_min}
#' forms the upper limit for the first cohort, and there is no lower
#' limit. If \code{open_first} is \code{FALSE}, then
#' \code{break_min} forms the lower limit for the first cohort.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate cohorts,
#' including cohorts that do not appear in the data.
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
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_year}},
#' \code{\link{date_to_cohort_multi}},
#' \code{\link{date_to_cohort_custom}},
#' and \code{\link{date_to_cohort_month}}.
#'
#' Other functions for working with one-quarter intervals are
#' \code{\link{date_to_age_quarter}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#'
#' See \code{\link{make_labels_period_quarter}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' \code{\link{plot_date_to_cohort_quarter}} creates plots
#' that illustrate how \code{date_to_cohort_quarter}
#' works.
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
#' @export
date_to_cohort_quarter <- function(date,
                                   break_min = NULL,
                                   open_first = NULL) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    if (has_break_min) {
        demcheck::err_length_1(x = break_min,
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
    ## deal with "empty" case where 'date'
    ## has length 0 or is all NA
    if (!has_date) {
        if (length(date) > 0L)
            ans <- factor(date,
                          levels = NA_character_,
                          exclude = NULL)
        else
            ans <- factor()
        return(ans)
    }
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_quarter(date = date,
                                       break_min = break_min)
    ## make labels for these breaks
    n_break <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n_break]]
    include_na <- anyNA(date)
    labels <- make_labels_cohort_quarter(break_min = break_min,
                                         break_max = break_max,
                                         open_first = open_first,
                                         include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans   
}

## HAS_TESTS
#' Convert dates to one-month cohorts
#'
#' Identify cohorts, based on dates of births or other events.
#' The cohorts have widths of one month.
#'
#' The interface for \code{date_to_cohort_month} is the similar
#' to that of \code{\link{date_to_period_month}}, except that
#' \code{date_to_cohort_month} also has arguments \code{break_min}
#' and \code{open_first}.
#' 
#' Supplying a date for \code{break_min} defines
#' the first cohort. It is similar
#' to supplying a value for \code{break_max} in
#' \code{\link{date_to_age_multi}}. If
#' \code{open_first} is \code{TRUE}, then \code{break_min}
#' forms the upper limit for the first cohort, and there is no lower
#' limit. If \code{open_first} is \code{FALSE}, then
#' \code{break_min} forms the lower limit for the first cohort.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate cohorts,
#' including cohorts that do not appear in the data.
#'
#' @inheritParams date_to_cohort_year
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating cohorts are
#' \code{\link{date_to_cohort_year}},
#' \code{\link{date_to_cohort_multi}},
#' \code{\link{date_to_cohort_custom}},
#' and \code{\link{date_to_cohort_quarter}}.
#'
#' Other functions for working with one-month intervals are
#' \code{\link{date_to_age_month}},
#' \code{\link{date_to_period_month}},
#' and \code{\link{date_to_triangle_month}}.
#'
#' See \code{\link{make_labels_period_month}} for the rules
#' on constructing labels for periods and cohorts.
#'
#' \code{\link{plot_date_to_cohort_month}} creates plots
#' that illustrate how \code{date_to_cohort_month}
#' works.
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
#' @export
date_to_cohort_month <- function(date,
                                 break_min = NULL,
                                 open_first = NULL) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    if (has_break_min) {
        demcheck::err_length_1(x = break_min,
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
    ## deal with "empty" case where 'date'
    ## has length 0 or is all NA
    if (!has_date) {
        if (length(date) > 0L)
            ans <- factor(date,
                          levels = NA_character_,
                          exclude = NULL)
        else
            ans <- factor()
        return(ans)
    }
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_month(date = date,
                                             break_min = break_min)
    ## make labels for these breaks
    n_break <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n_break]]
    include_na <- anyNA(date)
    labels <- make_labels_cohort_month(break_min = break_min,
                                       break_max = break_max,
                                       open_first = open_first,
                                       include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans   
}
