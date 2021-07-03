## HAS_TESTS
#' Put period labels into the format required
#' for single-year periods
#'
#' Given a vector of period labels, create a
#' \code{\link[base]{factor}} that contains
#' levels for the earliest and latest periods in \code{x},
#' and for all periods in between.
#' For instance, if the earliest period in \code{x}
#' is \code{"1990"}, and the latest is \code{"2010"},
#' then \code{format_period_year} creates a factor
#' with levels \code{"1990"}, \code{"1991"}, \dots,
#' \code{"2010"}.
#'
#' The elements of \code{x} must  be single-year
#' labels such as \code{"2001"} or \code{"2055"}.
#' \code{x} cannot contain multi-year intervals such
#' as \code{"2000-2005"} or \code{"<2020"}.
#'
#' As discussed in \code{\link{date_to_period_year}}, the
#' meaning of single-year labels for periods depends on the
#' the starting month, and whether periods are
#' labelled according to the calendar year
#' at the start of each period, or the calendar year
#' at the end (where these differ.) \code{format_period_year} leaves
#' labels untouched, so whatever convention
#' was used to create \code{x} also applies to the output
#' from \code{format_period_year}.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_period_year}
#' also contain \code{NA}.
#'
#' @param x A vector of single-year period labels.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' period labels are 
#' \code{\link{format_period_multi}},
#' \code{\link{format_period_custom}},
#' \code{\link{format_period_quarter}},
#' and \code{\link{format_period_month}}.
#'
#' \code{\link{date_to_period_year}} creates
#' single-year periods from dates.
#'
#' @examples
#' ## note that the 'levels' contain all values from
#' ## '2000' to '2010', even when these do not
#' ## appear in the data
#' format_period_year(x = c("2000", "2010"))
#'
#' format_period_year(x = c("2000", "2005", NA, "2004"))
#' @export 
format_period_year <- function(x) {
    format_period_month_quarter_year(x = x,
                                     parse_fun = parse_integers,
                                     labels_fun = make_labels_period_year)
}


## HAS_TESTS
#' Put period labels into the format required
#' for multi-year periods
#'
#' Given a vector of period labels, create a
#' \code{\link[base]{factor}}  containing
#' levels for the earliest and latest periods
#' in \code{x}, and for all periods in between.
#' For instance, if the earliest period in \code{x}
#' is \code{"1990-1995"}, and the latest is \code{"2005-2010"},
#' then \code{format_period_multi} creates a factor
#' with levels \code{"1990-1995"}, \code{"1995-2000"},
#' \code{"2000-2005"},and \code{"2005-2010"}.
#' All periods in the return value have the same width,
#' which is controlled by the \code{width} argument.
#'
#' The elements of \code{x} are typically multi-year
#' labels such as \code{"1950-1960"} or
#' \code{"2020-2025"} or single-year
#' labels such as \code{"2000"} or \code{"2025"}.
#' \code{x} cannot contain open intervals
#' such as \code{"<2020"}.
#'
#' As discussed in \code{\link{date_to_period_year}}, the
#' meaning of single-year labels for periods depends on the
#' the starting month, and whether periods are
#' labelled according to the calendar year
#' at the start of the period, or the calendar year
#' at the end. The multi-year labels produced
#' by \code{format_period_multi} are less ambiguous,
#' in that they always show a pair of years: the calendar
#' year at the start of the period and the calendar year at
#' the-end-of-the-period-plus-one-day. For instance, if a period
#' starts on 1 January 2000 and ends on 31 December 2000,
#' then the end of the period plus one day is 1 January 2001,
#' and the label is \code{"2000-2001"}.
#'
#' If \code{x} contains single-year labels, then \code{format_period_multi}
#' may need help to interpret these correctly. \code{format_period_multi}
#' assumes that periods start on January and that
#' single-year periods are labelled according to the calendar year
#' at the start of the period. Alternative settings can be
#' specified via the \code{month_start} and \code{label_start_year}
#' arguments.
#'
#' The location of the periods can be shifted
#' by using different values for \code{origin}.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_period_multi}
#' also contain \code{NA}.
#'
#' @param x A vector of period labels.
#' @param width The width, in whole years, of the periods
#' to be created. Defaults to 5.
#' @param origin An integer. Defaults to 2000.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Cohorts start on
#' the first day of this month.
#' @param label_year_start Logical. Whether to label a cohort
#' by the calendar year at the beginning of the cohort
#' or the calendar year at the end. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' period labels are 
#' \code{\link{format_period_year}},
#' \code{\link{format_period_custom}},
#' \code{\link{format_period_quarter}},
#' and \code{\link{format_period_month}}.
#'
#' \code{\link{date_to_period_year}} creates
#'  periods from dates.
#'
#' @examples
#' format_period_multi(x = c("2000-2001", "2005-2010", NA, "1996-1998"))
#'
#' ## use non-default value for 'width'
#' format_period_multi(x = c("2000-2001", "2005-2010", NA, "1996-1998"),
#'                     width = 10)
#'
#' ## use non-default value for 'origin' to shift periods
#' format_period_multi(x = c("2000-2001", "2005-2010", NA, "1996-1998"),
#'                     width = 10,
#'                     origin = 2001)
#'
#' ## Labels include single-year periods. Use default assumption
#' ## that single-year periods start in January and/or
#' ## are labelled by the calendar year at the start of
#' ## the period
#' format_period_multi(x = c("2000-2001", "2005", NA, "1996-1998"))
#'
#' ## Change to assumption that single-year periods are
#' ## start in July, and are labelled by the calendar
#' ## year at the end of the period
#' format_period_multi(x = c("2000-2001", "2005", NA, "1996-1998"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#' 
#' @export 
format_period_multi <- function(x,
                                width = 5, 
                                origin = 2000,
                                month_start = "Jan",
                                label_year_start = TRUE) {
    ## check arguments
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
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
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## parse the labels
    parsed <- parse_integers_intervals(x = labels_x,
                                       name = "x",
                                       month_start = month_start,
                                       label_year_start = label_year_start,
                                       label_open_multi = FALSE) # avoids error message
    low <- parsed$low # integer
    up <- parsed$up   # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_min_x <- parsed$break_min # integer
    break_max_x <- parsed$break_max # integer
    i_open <- match(TRUE, is_open_first | is_open_last, nomatch = 0L)
    if (i_open > 0L) {
        stop(gettextf("'%s' has open interval [\"%s\"]",
                      "x", labels_x[[i_open]]),
             call. = FALSE)
    }
    ## make 'break_min', 'break_max'
    remainder_min <- (break_min_x - origin) %% width
    break_min <- break_min_x - remainder_min
    remainder_max <- (break_max_x - origin) %% width
    if (remainder_max == 0L)
        break_max <- break_max_x
    else
        break_max <- break_max_x - remainder_max + width
    ## check that all intervals fall within implied breaks
    breaks <- seq.int(from = break_min,
                      to = break_max,
                      by = width)
    i_interval <- make_i_interval(low = low,
                                   up = up,
                                   breaks = breaks,
                                   open_first = FALSE,
                                   open_last = FALSE)
    is_multiple_intervals <- i_interval == -1L
    i_multiple_intervals <- match(TRUE, is_multiple_intervals, nomatch = 0L)
    if (i_multiple_intervals > 0L)
        stop(gettextf("label \"%s\" from '%s' intersects two or more intervals formed using %s = %d and %s = %d",
                      labels_x[[i_multiple_intervals ]], "x", "origin", origin, "width", width),
             call. = FALSE)
    ## make labels
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_period_custom(breaks = breaks,
                                            include_na = include_na)
    ## assign new labels to x and return
    ans <- labels_new[i_interval][match(x, labels_x)]
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Put period labels into the format required
#' for customised periods
#'
#' Given a vector of period labels, create a
#' \code{\link[base]{factor}}
#' that contains levels for all periods
#' defined by \code{breaks}. \code{format_period_custom}
#' is the most flexible
#' of the \code{format_period} functions
#' in that the periods can have any combination of widths,
#' though the widths must be defined in whole numbers of years.
#'
#' The elements of \code{x} can be
#' single-year labels such as \code{"2000"}
#' and \code{"1975"}, multi-year labels
#' such as \code{"1950-1960"} and \code{"2020-2025"},
#' or a mixture of the two.
#' \code{x} cannot contain open intervals
#' such as \code{"<2020"}.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_period_custom}
#' also contain \code{NA}.
#'
#' @inheritParams format_period_multi
#' @param breaks A vector of strictly increasing integer values.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' period labels are 
#' \code{\link{format_period_year}},
#' \code{\link{format_period_multi}},
#' \code{\link{format_period_quarter}},
#' and \code{\link{format_period_month}}.
#'
#' \code{\link{date_to_period_year}} creates
#' periods from dates.
#'
#' @examples
#' format_period_custom(x = c("2000-2001", "2004", "2005-2010", "1996-1998"),
#'                      breaks = c(1990, 2000, 2020))
#'
#' format_period_custom(x = c("2000-2001", "2004", "2005-2010", "1996-1998"),
#'                      breaks = c(1995, 2005, 2010, 2020))
#' @export 
format_period_custom <- function(x,
                                 breaks,
                                 month_start = "Jan",
                                 label_year_start = TRUE) {
    ## check arguments
    breaks <- demcheck::err_tdy_breaks_integer_period(breaks = breaks)
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    ## deal with "empty" case where 'breaks' has length 0
    n_break <- length(breaks)
    n_x <- length(x)
    if (n_break == 0L) {
        if (n_x > 0L) {
            stop(gettextf("'%s' has length %d",
                          "breaks", 0L),
                 call. = FALSE)
        }
        else {
            ans <- factor()
            return(ans)
        }
    }
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## parse the labels
    parsed <- parse_integers_intervals(x = labels_x,
                                       name = "x",
                                       month_start = month_start,
                                       label_year_start = label_year_start,
                                       label_open_multi = FALSE) # avoids error message
    low <- parsed$low # integer
    up <- parsed$up   # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_min_x <- parsed$break_min # integer
    break_max_x <- parsed$break_max # integer
    i_open <- match(TRUE, is_open_first | is_open_last, nomatch = 0L)
    if (i_open > 0L) {
        stop(gettextf("'%s' has open interval [\"%s\"]",
                      "x", labels_x[[i_open]]),
             call. = FALSE)
    }
    ## check intervals within bounds set by breaks
    is_too_low <- !is.na(low) & (low < breaks[[1L]])
    i_too_low <- match(TRUE, is_too_low, nomatch = 0L)
    if (i_too_low > 0L)
        stop(gettextf("interval '%s' starts below lowest value of '%s' [%d]",
                      labels_x[[i_too_low]], "breaks", breaks[[1L]]),
             call. = FALSE)
    is_too_high <- !is.na(up) & (up > breaks[[n_break]])
    i_too_high <- match(TRUE, is_too_high, nomatch = 0L)
    if (i_too_high > 0L)
        stop(gettextf("interval '%s' ends above highest value of '%s' [%d]",
                      labels_x[[i_too_high]], "breaks", breaks[[n_break]]),
             call. = FALSE)
    ## check that intervals do not cross boundaries set by breaks
    i_interval <- make_i_interval(low = low,
                                  up = up,
                                  breaks = breaks,
                                  open_first = FALSE,
                                  open_last = FALSE)
    is_multiple_intervals <- i_interval == -1L
    i_multiple_intervals <- match(TRUE, is_multiple_intervals, nomatch = 0L)
    if (i_multiple_intervals > 0L)
        stop(gettextf("label \"%s\" from '%s' intersects two or more intervals formed from '%s'",
                      labels_x[[i_multiple_intervals ]], "x", "breaks"),
             call. = FALSE)
    ## make labels for these breaks
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_period_custom(breaks = breaks,
                                            include_na = include_na)
    ## return result
    ans <- labels_new[i_interval][match(x, labels_x)]
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Put period labels into the format required
#' for quarter (three-month) periods
#'
#' Given a vector of period labels, create a
#' \code{\link[base]{factor}} that contains
#' levels for the earliest and latest periods in \code{x},
#' and for all periods in between.
#' For instance, if the earliest period in \code{x}
#' is \code{"1990 Q1"}, and the latest is \code{"2010 Q4"},
#' then \code{format_period_quarter} creates a factor
#' with levels \code{"1990 Q1"}, \code{"1990 Q2"}, \dots,
#' \code{"2010 Q3"}, \code{"2010 Q4"}.
#'
#' Quarters are defined as follows:
#' \tabular{lll}{
#'   \strong{Quarter} \tab \strong{Start} \tab \strong{End} \cr
#'   Q1 \tab 1 January \tab 31 March \cr
#'   Q2 \tab 1 April \tab 30 June \cr
#'   Q3 \tab 1 July \tab 30 September \cr
#'   Q4 \tab 1 October \tab 31 December
#' }
#'
#' The elements of \code{x} must all be single quarters,
#' eg \code{"2001 Q2"} or \code{"2055 Q1"}.
#' Open intervals such as \code{"<2001 Q2"} are not
#' allowed.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_period_quarter}
#' also contain \code{NA}.
#'
#' @param x A vector of period labels.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' period labels are 
#' \code{\link{format_period_year}},
#' \code{\link{format_period_multi}},
#' \code{\link{format_period_custom}},
#' and \code{\link{format_period_month}}.
#'
#' \code{\link{date_to_period_quarter}} creates
#' quarter periods from dates.
#'
#' @examples
#' format_period_quarter(x = c("2000 Q4", "2005 Q1", NA, "2004 Q3"))
#' @export 
format_period_quarter <- function(x) {
    format_period_month_quarter_year(x = x,
                                     parse_fun = parse_quarters,
                                     labels_fun = make_labels_period_quarter)
}


## HAS_TESTS
#' Put period labels into the format required
#' for one-month periods
#'
#' Given a vector of period labels, create a
#' \code{\link[base]{factor}} that contains
#' levels for the earliest and latest periods in \code{x},
#' and for all periods in between.
#' For instance, if the earliest period in \code{x}
#' is \code{"1990 Jan"}, and the latest is \code{"2010 Dec"},
#' then \code{format_period_month} creates a factor
#' with levels \code{"1990 Jan"}, \code{"1990 Feb"}, \dots,
#' \code{"2010 Nov"}, \code{"2010 Dec"}.
#'
#' The elements of \code{x} must all be single months,
#' eg \code{"2001 Feb"} or \code{"2055 Mar"}.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_period_month}
#' also contain \code{NA}.
#'
#' @param x A vector of period labels.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' period labels are 
#' \code{\link{format_period_year}},
#' \code{\link{format_period_multi}},
#' \code{\link{format_period_custom}},
#' and \code{\link{format_period_quarter}}.
#'
#' \code{\link{date_to_period_month}} creates
#' month periods from dates.
#'
#' @examples
#' format_period_month(x = c("2000 Sep", "2005 May", NA, "2004 Jan"))
#' @export 
format_period_month <- function(x) {
    format_period_month_quarter_year(x = x,
                                     parse_fun = parse_months,
                                     labels_fun = make_labels_period_month)
}
