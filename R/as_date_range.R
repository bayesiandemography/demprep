
## HAS_TESTS
#' Convert standard labels for one-year periods
#' or cohorts to labels specifying precise dates
#'
#' Convert standard labels for one-year periods
#' such as "2020", "2001", or "<2005" to
#' labels that give precise date ranges
#' such as \code{"[2020-01-01, 2020-12-31]"},
#' \code{"[2001-10-01, 2002-10-01]"},
#' or \code{"(-Inf, , 2005-06-30]"}.
#'
#' The vector of period or cohort labels \code{x} is typically
#' created by functions
#' \code{\link{format_period_year}},
#' or \code{\link{format_cohort_year}}.
#'
#' The date-range format can be helpful
#' when bringing together data sources that
#' use different conventions for labelling
#' periods and cohorts.
#'
#' Arguments \code{month_start} specifies the
#' date on which period or cohort starts, and
#' \code{label_year_start} specifies whether
#' labels are based on the calendar year at the
#' start or end of each period or cohort.
#' See the \code{\link{date_to_period_year}}
#' or \code{\link{date_to_cohort_year}} for details.
#' 
#' @param x A vector of period or cohort labels.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Each period starts on
#' the first day of this month.
#' @param label_year_start Whether to label a period
#' by the calendar year at the beginning of the period
#' or the calendar year at the end. Defaults to \code{TRUE}.
#'
#' @return A factor the same length as \code{x}.
#'
#' @seealso 
#' \code{\link{as_date_range_multi}},
#' \code{\link{as_date_range_custom}},
#' \code{\link{as_date_range_quarter}},
#' \code{\link{as_date_range_month}}
#' 
#' @examples
#' x <- c("<2010", "2010", "2011")
#' as_date_range_year(x)
#' as_date_range_year(x,
#'                    month_start = "Jul")
#' as_date_range_year(x,
#'                    month_start = "Jul",
#'                    label_year_start = FALSE)
#' @export
as_date_range_year <- function(x,
                               month_start = "Jan",
                               label_year_start = TRUE) {
    ## check arguments
    if (!is.vector(x))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    ## deal with "empty" cases where 'x'
    ## has length 0 or is all NA
    if (length(x) == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        ans <- factor(x,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'levels_x' vector
    if (is.factor(x))
        levels_x <- levels(x)
    else
        levels_x <- unique(x)
    ## parse the labels
    parsed <- parse_integers(x = x,
                             name = "x")
    year_low <- parsed$low
    year_up <- parsed$up
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", levels_x[[i_open_last]]),
             call. = FALSE)
    }
    ## create dates
    subtract_1 <- (month_start != "Jan") && !label_year_start
    if (subtract_1) {
        year_low <- year_low - 1L
        year_up <- year_up - 1L
    }
    date_low <- ifelse(is.na(year_low),
                       NA,
                       paste(year_low, month_start, 1))
    date_up <- ifelse(is.na(year_up),
                      NA,
                      paste(year_up, month_start, 1))
    date_low <- as.Date(date_low, format = "%Y %b %d")
    date_up <- as.Date(date_up, format = "%Y %b %d")
    ## make new labels
    x_new <- mapply(c, date_low, date_up, SIMPLIFY = FALSE)
    levels_x_new <- make_labels_dateranges(x_new)
    ## put in order
    i <- order_low_up(low = year_low,
                      up = year_up)
    levels_x_ordered <- levels_x[i]
    levels_x_new_ordered <- levels_x_new[i]
    ## make return value
    ans <- factor(x,
                  levels = levels_x_ordered,
                  labels = levels_x_new_ordered,
                  exclude = NULL)
    ans
}


#' Convert standard labels for multi-year periods
#' or cohorts to labels specifying precise dates
#'
#' Convert standard labels for multi-year periods
#' of equal width
#' such as \code{"<2020"}, \code{"2020-2025"},
#' \code{"2025-2030"}
#' to labels that give precise date ranges
#' such as \code{"(-Inf, 2020-06-30]"},
#' \code{"[2020-07-01, 2025-06-30]"},
#' \code{"[2025-07-01, , 2030-06-30]"}.
#'
#' The vector of period or cohort labels
#' \code{x} is typically
#' created by functions
#' \code{\link{format_period_multi}},
#' or \code{\link{format_cohort_multi}}.
#'
#' The date-range format can be helpful
#' when bringing together data sources that
#' use different conventions for labelling
#' periods and cohorts.
#'
#' Arguments \code{month_start} specifies the
#' date on which periods or cohorts start.
#' For instance, when \code{month_start} is
#' \code{"Jan"}, the default, each period or
#' cohort starts on 1 January. When 
#' \code{month_start} is \code{"Mar"},
#' each cohort starts on 1 March.
#' 
#' @inheritParams as_date_range_year
#'
#' @return A factor the same length as \code{x}.
#'
#' @seealso 
#' \code{\link{as_date_range_year}},
#' \code{\link{as_date_range_custom}},
#' \code{\link{as_date_range_quarter}},
#' \code{\link{as_date_range_month}}
#'
#' @examples
#' x <- c("<2010", "2010-2015", "2015-2020")
#' as_date_range_multi(x)
#' as_date_range_multi(x, month_start = "Jul")
#' @export
as_date_range_multi <- function(x,
                                month_start = "Jan") {
    ## check arguments
    if (!is.vector(x))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    ## deal with "empty" cases where 'x'
    ## has length 0 or is all NA
    if (length(x) == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        ans <- factor(x,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'levels_x' vector
    if (is.factor(x))
        levels_x <- levels(x)
    else
        levels_x <- unique(x)
    ## parse the labels
    parsed <- parse_intervals(x = levels_x,
                              name = "x")
    year_low <- parsed$low
    year_up <- parsed$up
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", levels_x[[i_open_last]]),
             call. = FALSE)
    }
    ## check that widths equal
    widths_low_up <- year_up - year_low
    i_non_na <- match(FALSE, is.na(widths_low_up), nomatch = 0L)
    if (i_non_na > 0L) {
        width <- widths_low_up[[i_non_na]]
        i_unequal_width <- match(FALSE, width == widths_low_up, nomatch = 0L)
        if (i_unequal_width > 0L)
            stop(gettextf("intervals \"%s\" and \"%s\" in '%s' have different widths",
                          levels_x[[i_non_na]], levels_x[[i_unequal_width]], "x"))
        lowup <- as.integer(rbind(year_low, year_up))
        diff_lowup <- diff(lowup)
        is_uneven <- (diff_lowup %% width) != 0L
        i_uneven <- match(TRUE, is_uneven, nomatch = 0L)
        if (i_uneven > 0L) {
            i_first <- i_uneven / 2L
            stop(gettextf("gaps between intervals \"%s\" and \"%s\" in '%s' not divisible by width of intervals [%d]",
                          levels_x[[i_first]],
                          levels_x[[i_first + 1L]],
                          "x",
                          width))
        }
    }
    ## create dates
    date_low <- ifelse(is.na(year_low),
                       NA,
                       paste(year_low, month_start, 1))
    date_up <- ifelse(is.na(year_up),
                      NA,
                      paste(year_up, month_start, 1))
    date_low <- as.Date(date_low, format = "%Y %b %d")
    date_up <- as.Date(date_up, format = "%Y %b %d")
    ## make new labels
    x_new <- mapply(c, date_low, date_up, SIMPLIFY = FALSE)
    levels_x_new <- make_labels_dateranges(x_new)
    ## put in order
    i <- order_low_up(low = year_low,
                      up = year_up)
    levels_x_ordered <- levels_x[i]
    levels_x_new_ordered <- levels_x_new[i]
    ## make return value
    ans <- factor(x,
                  levels = levels_x_ordered,
                  labels = levels_x_new_ordered,
                  exclude = NULL)
    ans
}
    

#' Convert standard labels for customised periods
#' or cohorts to labels specifying precise dates
#'
#' Convert standard labels for periods
#' or cohorts with varying widths
#' (denominated in years), such as
#' such as \code{"<2020"}, \code{"2020-2030"},
#' \code{"2030-2032"}
#' to labels that give precise date ranges
#' such as \code{"(-Inf, 2020-06-30]"},
#' \code{"[2020-07-01, 2030-06-30]"},
#' \code{"[2030-07-01, , 2032-06-30]"}.
#'
#' The vector of period or cohort labels
#' \code{x} is typically
#' created by functions
#' \code{\link{format_period_custom}},
#' or \code{\link{format_cohort_custom}}.
#'
#' The date-range format can be helpful
#' when bringing together data sources that
#' use different conventions for labelling
#' periods and cohorts.
#'
#' Arguments \code{month_start} specifies the
#' date on which periods or cohorts start.
#' For instance, when \code{month_start} is
#' \code{"Jan"}, the default, each period or
#' cohort starts on 1 January. When 
#' \code{month_start} is \code{"Mar"},
#' each cohort starts on 1 March.
#' 
#' @inheritParams as_date_range_year
#'
#' @return A factor the same length as \code{x}.
#'
#' @seealso 
#' \code{\link{as_date_range_year}},
#' \code{\link{as_date_range_multi}},
#' \code{\link{as_date_range_quarter}},
#' \code{\link{as_date_range_month}}
#' 
#' @examples
#' x <- c("<2010", "2010-2012", "2012-2020")
#' as_date_range_custom(x)
#' as_date_range_custom(x, month_start = "Jul")
#' @export
as_date_range_custom <- function(x,
                                 month_start = "Jan") {
    ## check arguments
    if (!is.vector(x))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    ## deal with "empty" cases where 'x'
    ## has length 0 or is all NA
    if (length(x) == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        ans <- factor(x,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'levels_x' vector
    if (is.factor(x))
        levels_x <- levels(x)
    else
        levels_x <- unique(x)
    ## parse the labels
    parsed <- parse_intervals(x = levels_x,
                              name = "x")
    year_low <- parsed$low
    year_up <- parsed$up
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", levels_x[[i_open_last]]),
             call. = FALSE)
    }
    ## create dates
    date_low <- ifelse(is.na(year_low),
                       NA,
                       paste(year_low, month_start, 1))
    date_up <- ifelse(is.na(year_up),
                      NA,
                      paste(year_up, month_start, 1))
    date_low <- as.Date(date_low, format = "%Y %b %d")
    date_up <- as.Date(date_up, format = "%Y %b %d")
    ## make new labels
    x_new <- mapply(c, date_low, date_up, SIMPLIFY = FALSE)
    levels_x_new <- make_labels_dateranges(x_new)
    ## put in order
    i <- order_low_up(low = year_low,
                      up = year_up)
    levels_x_ordered <- levels_x[i]
    levels_x_new_ordered <- levels_x_new[i]
    ## make return value
    ans <- factor(x,
                  levels = levels_x_ordered,
                  labels = levels_x_new_ordered,
                  exclude = NULL)
    ans
}


#' Convert standard labels for one-quarter periods
#' or cohorts to labels specifying precise dates
#'
#' Convert standard labels for one-quarter
#' (three-month) periods
#' or cohorts, such as
#' such as \code{"<2020 Q2"}, \code{"2020 Q2"},
#' \code{"2020 Q3"}
#' to labels that give precise date ranges
#' such as \code{"(-Inf, 2020-03-31]"},
#' \code{"[2020-04-01, 2020-06-30]"},
#' \code{"[2020-07-01, 2020-10-31]"}.
#'
#' The vector of period or cohort labels
#' \code{x} is typically
#' created by functions
#' \code{\link{format_period_quarter}},
#' or \code{\link{format_cohort_quarter}}.
#'
#' @inheritParams as_date_range_year
#'
#' @return A factor the same length as \code{x}.
#'
#' @seealso \code{\link{as_date_range_year}},
#' \code{\link{as_date_range_multi}},
#' \code{\link{as_date_range_custom}},
#' \code{\link{as_date_range_month}}.
#'
#' @examples
#' x <- c("<2010 Q4", "2010 Q4", "2011 Q1")
#' as_date_range_quarter(x)
#' @export
as_date_range_quarter <- function(x) {
    as_date_range_month_quarter(x = x,
                                parse_fun = parse_quarters)
}


#' Convert standard labels fopr one-month periods
#' or cohorts to labels specifying precise dates
#'
#' Convert standard labels for one-month
#' periods or cohorts, such as
#' such as \code{"<2020 Jan"}, \code{"2020 Jan"},
#' \code{"2020 Feb"}
#' to labels that give precise date ranges
#' such as \code{"(-Inf, 2020-03-31]"},
#' \code{"[2020-04-01, 2020-06-30]"},
#' \code{"[2020-07-01, 2020-10-31]"}.
#'
#' The vector of period or cohort labels
#' \code{x} is typically
#' created by functions
#' \code{\link{format_period_quarter}},
#' or \code{\link{format_cohort_quarter}}.
#'
#' @inheritParams as_date_range_year
#'
#' @return A factor the same length as \code{x}.
#'
#' @seealso \code{\link{as_date_range_year}},
#' \code{\link{as_date_range_multi}},
#' \code{\link{as_date_range_custom}},
#' \code{\link{as_date_range_quarter}}
#'
#' @examples
#' x <- c("<2010 Dec", "2010 Dec", "2011 Jan")
#' as_date_range_month(x)
#' @export
as_date_range_month <- function(x) {
    as_date_range_month_quarter(x = x,
                                parse_fun = parse_months)
}

