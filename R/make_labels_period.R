
## HAS_TESTS
#' Make labels for periods measured in years
#'
#' Make labels for periods, with lengths measured in whole years.
#'
#' 
#' Periods are defined by the dates specified in \code{breaks}.
#' The typical period starts at one date given in \code{breaks}
#' and ends the day before the next date.
#' For instance, when the breaks are
#' \code{"2000-01-01", "2001-01-01", "2002-01-01"}
#' the first period extends starts on 2000-01-01, 
#' and ends on 2000-12-31, and the second period starts on
#' 2001-01-01 and ends on 2002-12-31.
#'
#' When \code{open_first} is \code{TRUE}, an extra 'open' period
#' extending indefinitely into the past is appended to the start
#' of the series. When \code{open_last} is \code{TRUE},
#' an extra period extending indefintely into the future is
#' appended to the end of the series. By default,
#' \code{open_first} and \code{open_last} are both \code{FALSE}.
#'
#' The labels constructed by \code{make_labels_period}
#' try to reflect demographic conventions.
#' If two breaks are exactly one year apart, then the
#' corresponding label consists of a single year, eg
#' \code{"2020"} or \code{"1951"}. If two breaks are
#' more than one year apart, then the label consists of
#' two years, separated by a dash, eg \code{"2020-2025"}
#' or \code{"1951-1961"}.
#'
#' Unfortunately, single-year labels can be ambiguous.
#' If a period starts on 2020-01-01 and ends
#' on 2020-12-31, then it is obvious that the single-year
#' label should be \code{"2020"}. But what about a year
#' that starts on 2020-07-01 and ends on 2021-06-30?
#' Some authorities use the calendar year at the \emph{start} of
#' the period, so that the label would be
#' \code{"2020"}. Other authorities use the calendar
#' year at the \emph{end} of the period, so that the label
#' would be \code{"2021"}. (In the latter case,
#' the years are often called "year to", eg "year to June".)
#'
#' \code{make_label_period} by default labels periods according
#' to the calendar year at the start of the period. To label
#' periods according the the calendar year at the end of the
#' period, use set \code{label_year_start} to \code{FALSE}.
#'
#' \code{label_year_start} can only be \code{FALSE} if
#' \code{open_first} and \code{open_last} are both \code{FALSE}.
#'
#' When \code{include_na} is \code{TRUE}, an \code{NA}
#' is added to the end of the labels. This can be useful
#' when dealing with data that include \code{NA}s.
#'
#' \code{breaks} can have length 0, provided that \code{open_first}
#' and \code{open_last} are both \code{FALSE}. It can
#' have length 1, provided that at last one of \code{open_first}
#' and \code{open_last} are TRUE.
#'
#' @param breaks A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param open_first Whether to append an open-ended
#' period to the start of the labels.
#' Defaults to \code{FALSE}.
#' @param open_last Whether to append an open-ended
#' period to the end of the labels.
#' Defaults to \code{FALSE}.
#' @param label_year_start Whether to label a period
#' by the calendar year at the beginning of the period.
#' Not needed for multi-year periods, or for single-year
#' periods that start on 1 January.
#' Defaults to \code{FALSE}.
#' @param include_na  Whether to append an \code{NA} to
#' the end of the labels. Defaults to \code{FALSE}.
#'
#' @return A character value. If \code{breaks} has length 0,
#' then the return value also has length 0; otherwise
#' it has length \code{length(breaks) - 1 + open_first + open_last}.
#'
#' @seealso To make labels for age groups, use
#' \code{\link{make_labels_age_group}}. There is
#' no \code{make_labels_cohort} function. To construct
#' labels for cohorts, just use \code{make_labels_period}.
#' To make labels for periods of one quarter or on month,
#' use functions \code{make_label_period_quarter} or
#' \code{make_label_period_month}.
#'
#' @examples
#' ## 5-year periods
#' make_labels_period(breaks = c("2005-01-01",
#'                               "2010-01-01",
#'                               "2015-01-01",
#'                               "2020-01-01"))
#' 
#' ## one-year periods, using default setting
#' ## where label based on calender year at start 
#' make_labels_period(breaks = c("2005-07-01",
#'                               "2006-07-01",
#'                               "2007-07-01"))
#' 
#' ## this time with label based on
#' ## calendar year at end
#' make_labels_period(breaks = c("2005-07-01",
#'                               "2006-07-01",
#'                               "2007-07-01"))
#'
#' ## add an open period at the beginning...
#' make_labels_period(breaks = c("2005-01-01",
#'                               "2010-01-01",
#'                               "2015-01-01",
#'                               "2020-01-01"),
#'                    open_first = TRUE)
#'
#' ## ...and at the end
#' make_labels_period(breaks = c("2005-01-01",
#'                               "2010-01-01",
#'                               "2015-01-01",
#'                               "2020-01-01"),
#'                    open_first = TRUE,
#'                    open_last = TRUE)
#'
#' ## add an 'NA' label
#' make_labels_period(breaks = c("2005-07-01",
#'                               "2006-07-01",
#'                               "2007-07-01"),
#'                    include_na = TRUE)
#' @export
make_labels_period <- function(breaks,
                               open_first = FALSE,
                               open_last = FALSE,
                               label_year_start = TRUE,
                               include_na = FALSE) {
    breaks <- demcheck::err_tdy_breaks_date(x = breaks,
                                            name = "breaks",
                                            open_first = open_first,
                                            open_last = open_last)
    demcheck::err_is_first_day_unit_vector(x = breaks,
                                           name = "breaks",
                                           unit = "year")
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    if (!label_year_start) {
        if (open_first)
            stop(gettextf("'%s' is %s but '%s' is %s",
                          "open_first", "TRUE", "label_year_start", "FALSE"),
                 call. = FALSE)
        if (open_last)
            stop(gettextf("'%s' is %s but '%s' is %s",
                          "open_last", "TRUE", "label_year_start", "FALSE"),
                 call. = FALSE)
    }
    n <- length(breaks)
    if (n == 0L) {
        ## 'err_tdy_breaks_date' checked that 'open_first' and 'open_last' both FALSE
        ans_mid <- character()
        ans_left <- NULL
        ans_right <- NULL
    }
    else if (n == 1L) {
        ## 'err_tdy_breaks_date' checked that 'open_first' or 'open_last' TRUE
        ans_mid <- format(breaks, "%Y")
        if (open_first)
            ans_left <- paste0("<", ans_mid)
        else
            ans_left <- NULL
        if (open_last)
            ans_right <- paste0(ans_mid, "+")
        else
            ans_right <- NULL
    }
    else {
        ## 'open_first' or 'open_last' may be TRUE
        ans_mid <- character(length = n - 1L)
        breaks_year <- as.integer(format(breaks, "%Y"))
        diff <- diff(breaks_year)
        is_single_year <- diff == 1L
        head <- breaks_year[-n]
        tail <- breaks_year[-1L]
        is_1_jan <- identical(format(breaks[[1L]], "%m-%d"), "01-01")
        use_head_for_single <- label_year_start || is_1_jan
        if (use_head_for_single)
            ans_mid[is_single_year] <- head[is_single_year]
        else
            ans_mid[is_single_year] <- tail[is_single_year]
        if (any(!is_single_year)) {
            lower <- head[!is_single_year]
            upper <- tail[!is_single_year]
            ans_mid[!is_single_year] <- paste(lower, upper, sep = "-")
        }
        if (open_first)
            ans_left <- paste0("<", head[[1L]])
        else
            ans_left <- NULL
        if (open_last)
            ans_right <- paste0(tail[[n - 1L]], "+")
        else
            ans_right <- NULL
    }
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    c(ans_left, ans_mid, ans_right, ans_na)
}





#' @export
make_labels_period_quarter <- function(break_min,
                                       break_max,
                                       open_first = FALSE,
                                       open_last = FALSE,
                                       include_na = FALSE) {
    make_labels_period_month_quarter(break_min = break_min,
                                     break_max = break_max,
                                     open_first = open_first,
                                     open_last = open_last,
                                     unit = "quarter",
                                     include_na)
}




#' @export
make_labels_period_month <- function(break_min,
                                     break_max,
                                     open_first = FALSE,
                                     open_last = FALSE,
                                     include_na = FALSE) {
    make_labels_period_month_quarter(break_min = break_min,
                                     break_max = break_max,
                                     open_first = open_first,
                                     open_last = open_last,
                                     unit = "month",
                                     include_na)
}


