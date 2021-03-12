
## Note - all functions return empty factor when supplied with
## empty inputs, except for 'date_to_period_custom', which
## returns factor with levels implied by 'breaks',
## (reflecting the fact that the levels are
## completely determined by this argument).

## HAS_TESTS
#' Convert dates to one-year periods
#'
#' Allocate dates to one-year periods. The one-year periods are,
#' by default, calendar years.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' Periods start on the first day of \code{month_start},
#' and end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' If a period starts on 1 January, then the first day and last day
#' of the period belong to the same calendar year.
#' But if a period starts on any other day, then
#' the first day belongs to one calendar
#' year and the last day belongs to the next calendar year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, then the first
#' day belongs to the year 2000, and the last
#' day belongs to the year 2001. Some authors
#' use the first year to label such periods,
#' and others use the last year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, some authors
#' label the period \code{"2000"}, and
#' others label it \code{"2001"}. Function
#' \code{date_to_period_year} by default uses
#' the start year. To use the end year, set
#' \code{label_year_start} to \code{FALSE}.
#'
#' The return value is a factor.
#' The levels of this factor include all intermediate periods,
#' including periods that do not appear in the data.
#'
#' @param date Dates of events or measurements.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Each period starts on
#' the first day of this month.
#' @param label_year_start Whether to label a period
#' by the calendar year at the beginning of the period
#' or the calendar year at the end. Not needed for periods
#' that start on 1 January. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_custom}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_period_month}}.
#'
#' Other functions for working with one-year intervals are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#'
#' \code{\link{make_labels_period}} describes the rules
#' for constructing labels for periods.
#'
#' \code{\link{plot_date_to_period_year}} creates plots
#' that illustrate how \code{date_to_period_year}
#' works.
#'
#' @examples
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## period starts on 1 July, not 1 January
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul")
#'
#' ## period starts on 1 July, using the calendar year at
#' ## the end, rather than beginning, for the label
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#' @export
date_to_period_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
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
                                            break_min = NULL)
    ## create labels
    include_na <- anyNA(date)
    labels <- make_labels_period(breaks = breaks,
                                 label_year_start = label_year_start,
                                 include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to multi-year periods
#'
#' Allocate dates to multi-year periods. The periods all have
#' the same width, which by default is 5 years.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' Periods are \code{width} years long.
#' They start on the first day of \code{month_start},
#' and end \code{width}-years-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' The location of the periods can be shifted
#' by using different values for \code{origin}.
#' 
#' The return value is a factor.
#' The levels of this factor include all intermediate periods,
#' including periods that do not appear in the data.
#'
#' @inheritParams date_to_period_year
#' @param width The length, in whole years, of the periods.
#' Defaults to 5.
#' @param origin An integer. Defaults to 2000. 
#' 
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_period_custom}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_period_month}}.
#'
#' Other functions for working with multi-year intervals are
#' \code{\link{date_to_age_multi}},
#' \code{\link{date_to_cohort_multi}},
#' and \code{\link{date_to_triangle_multi}}.
#'
#' \code{\link{make_labels_period}} describes the rules
#' for constructing labels for periods.
#'
#' \code{\link{plot_date_to_period_multi}} creates plots
#' that illustrate how \code{date_to_period_multi}
#' works.
#'
#' @examples
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"))
#'
#' ## width is 10
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      width = 5)
#'
#' ## origin is 2001
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      origin = 2001)
#'
#' ## period starts on 1 July, not 1 January
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      origin = 2001,
#'                      month_start = "Jul")
#' @export
date_to_period_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 month_start = "Jan") {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
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
                                    break_min = NULL)
    ## make labels for these breaks
    include_na <- anyNA(date)
    labels <- make_labels_cohort(breaks = breaks,
                                 open_first = FALSE,
                                 label_year_start = NULL,
                                 include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to customized periods
#'
#' Allocate dates to periods with varying widths,
#' though all widths are measured in whole years.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' \code{breaks} is also vector of class \code{\link[base]{Date}},
#' or can be coerced to to one. \code{breaks} is
#' used to define the points where each period starts and finishes.
#' The dates in \code{breaks} must all be the first day of
#' the same month of the year. For instance,
#' \code{breaks} could consist of
#' the values \code{"2010-01-01"} and \code{"2017-01-01"},
#' but not \code{"2010-01-01"} and \code{"2017-01-02"},
#' or \code{"2010-01-01"} and \code{"2017-02-01"}.
#' The final period finishes
#' one day before the final break.
#' If, for instance, \code{breaks} has values \code{"2010-01-01"},
#' \code{"2017-01-01"}, and \code{"2021-01-01"}, then the first period
#' starts on 1 January 2010 and ends on 31 December 2016,
#' and the second period starts on 1 January 2017 and ends
#' on 31 December 2020.
#'
#' The return value is a factor.
#' The levels of this factor include all intermediate periods,
#' including periods that do not appear in the data.
#'
#' @inheritParams date_to_period_year
#' @param breaks Dates defining starts and ends of periods.
#' 
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_period_month}}.
#'
#' Other functions for working with customized intervals are
#' \code{\link{date_to_age_custom}},
#' and \code{\link{date_to_cohort_custom}}.
#'
#' \code{\link{make_labels_period}} describes the rules
#' for constructing labels for periods.
#'
#' \code{\link{plot_date_to_period_custom}} creates plots
#' that illustrate how \code{date_to_period_custom}
#' works.
#'
#' @examples
#' ## periods start on 1 January
#' date_to_period_custom(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                       breaks = c("2000-01-01",
#'                                  "2019-01-01",
#'                                  "2026-01-01"))
#'
#' ## periods start on 1 March
#' date_to_period_custom(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                       breaks = c("2000-03-01",
#'                                  "2019-03-01",
#'                                  "2026-03-01"))
#' @export
date_to_period_custom <- function(date,
                                  breaks) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    breaks <- demcheck::err_tdy_breaks_date_period(breaks = breaks)
    n <- length(breaks)
    if (n > 0L) {
        break_min <- breaks[[1L]]
        break_max <- breaks[[n]]
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
        demcheck::err_lt_break_max_date(date = date,
                                        break_max = break_max)
    }
    ## deal with "empty" case where 'breaks' has length 0
    if (n == 0L) {
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
    labels <- make_labels_period(breaks = breaks,
                                 label_year_start = NULL,
                                 include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to one-quarter periods
#'
#' Allocate dates to periods with periods one quarter long.
#' Quarters are defined as follows:
#' \tabular{lll}{
#'   \strong{Quarter} \tab \strong{Start} \tab \strong{End} \cr
#'   Q1 \tab 1 January \tab 31 March \cr
#'   Q2 \tab 1 April \tab 30 June \cr
#'   Q3 \tab 1 July \tab 30 September \cr
#'   Q4 \tab 1 October \tab 31 December
#' }
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' The return value is a factor.
#' The levels of this factor include all intermediate periods,
#' including periods that do not appear in the data.
#'
#' @inheritParams date_to_period_year
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_custom}},
#' and \code{\link{date_to_period_month}}.
#'
#' Other functions for working with quarter intervals are
#' \code{\link{date_to_age_quarter}},
#' and \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#'
#' \code{\link{make_labels_period_quarter}} describes the rules
#' for constructing labels for quarter periods.
#'
#' \code{\link{plot_date_to_period_quarter}} creates plots
#' that illustrate how \code{date_to_period_quarter}
#' works.
#' 
#' @examples
#' date_to_period_quarter(date = c("2024-03-27",
#'                                 "2020-01-03",
#'                                 "2022-11-09"))
#' @export
date_to_period_quarter <- function(date) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    ## deal with "empty" case where 'date' has length 0
    ## or is all NA
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
                                               break_min = NULL)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    include_na <- anyNA(date)
    labels <- make_labels_period_quarter(break_min = break_min,
                                         break_max = break_max,
                                         include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans   
}

## HAS_TESTS
#' Convert dates to one-month periods
#'
#' Allocate dates to periods with month-long
#' periods.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' The return value is a factor.
#' The levels of this factor include all intermediate periods,
#' including periods that do not appear in the data.
#'
#' @inheritParams date_to_period_year
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_custom}},
#' and \code{\link{date_to_period_quarter}}.
#'
#' Other functions for working with month intervals are
#' \code{\link{date_to_age_month}},
#' and \code{\link{date_to_cohort_month}},
#' and \code{\link{date_to_triangle_month}}.
#'
#' \code{\link{make_labels_period_month}} describes the rules
#' for constructing labels for month periods.
#'
#' \code{\link{plot_date_to_period_month}} creates plots
#' that illustrate how \code{date_to_period_month}
#' works.
#'
#' @examples
#' date_to_period_month(date = c("2024-03-27",
#'                               "2020-01-03",
#'                               "2022-11-09"))
#' @export
date_to_period_month <- function(date) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    ## deal with "empty" case where 'date' has length 0
    ## or all is NA
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
                                             break_min = NULL)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    include_na <- anyNA(date)
    labels <- make_labels_period_month(break_min = break_min,
                                       break_max = break_max,
                                       include_na = include_na)
    ## assign labels to dates
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans   
}
