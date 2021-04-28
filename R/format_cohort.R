
## HAS_TESTS
#' Put cohort labels into the format required
#' for single-year cohorts
#'
#' Given a vector of cohort labels, create a
#' \code{\link[base]{factor}} where the levels
#' contain a complete set of cohorts. 
#' If \code{open_first} is \code{TRUE}, then the earliest
#' cohort has no lower limit. (This is equivalent
#' to an open age group with no upper limit.)
#' 
#' The elements of \code{x} must be single-year
#' labels, such as \code{"2001"}, or
#' labels for cohorts
#' that are open on the left, such as \code{"<2000"}.
#' \code{x} must not contain
#' multi-year intervals such
#' as \code{"2000--2005"}
#'
#' \code{open_first} defaults to \code{TRUE}
#' if a value for \code{break_min} is supplied,
#' or if any elements of \code{x} are open
#' on the left, and to \code{FALSE} otherwise.
#'
#' If \code{x} has \code{NA}s, then the
#' levels of the factor created by \code{format_cohort_year}
#' will included an \code{NA}.
#'
#' @param x A vector of cohort labels.
#' @param break_min An integer. The lower limit of the cohort,
#' or \code{NULL} (the default).
#' @param open_first Whether the first cohort
#' has no lower limit. 
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformatting
#' cohort labels are 
#' \code{\link{format_cohort_multi}},
#' \code{\link{format_cohort_custom}},
#' \code{\link{format_cohort_quarter}},
#' and \code{\link{format_cohort_month}}.
#'
#' \code{\link{date_to_cohort_year}} creates
#' single-year cohorts from dates.
#'
#' \code{\link{make_labels_cohort}} describes the rules
#' for constructing labels for cohorts.
#'
#' @examples
#' format_cohort_year(x = c("2000", "2005", NA, "2004"))
#'
#' ## 'x' contains an open interval, so
#' ## 'open_first' defaults to TRUE
#' format_cohort_year(x = c("<2000", "2005", NA, "2004"))
#'
#' ## specify 'break_min', which also makes
#' ## 'open_first' default to TRUE
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 1998)
#'
#' ## set 'break_min' above minimum value of 'x'
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 2001)
#'
#' ## set 'open_first' to FALSE 
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    open_first = FALSE)
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 1998,
#'                    open_first = FALSE)
#'
#' ## if no value for 'break_min' is supplied, and one or
#' ## more of the labels refers to an open cohort,
#' ## then the levels start with the latest open cohort
#' format_cohort_year(x = c("2000", "2005", "<2002",
#'                          "2004", "<2001"))
#' @export 
format_cohort_year <- function(x,
                               break_min = NULL,
                               open_first = NULL) {
    format_cohort_month_quarter_year(x = x,
                                     break_min = break_min,
                                     open_first = open_first,
                                     parse_fun = parse_integers,
                                     label_fun = make_labels_cohort_year)
}


## HAS_TESTS
#' Put cohort labels into the format required
#' for multi-year cohorts
#'
#' Given a vector of cohort labels, create a
#' \code{\link[base]{factor}}  containing
#' levels for the earliest and latest cohorts
#' in \code{x}, and for all cohorts in between.
#' For instance, if the earliest cohort in \code{x}
#' is \code{"1990--1995"}, and the latest is \code{"2005--2010"},
#' then \code{format_cohort_multi} creates a factor
#' with levels \code{"1990--1995"}, \code{"1995--2000"},
#' \code{"2000--2005"}, and \code{"2005--2010"}.
#' All cohorts, with the possible exception of
#' a first "open" cohort, have the same length.
#' The length of cohorts is controlled by
#' \code{width}.
#'
#' If \code{open_first} is \code{TRUE}, then the earliest
#' cohort has no lower limit. (This is equivalent
#' to an open age group with no upper limit.)
#' 
#' The elements of \code{x} can be
#' single-year labels such as \code{"2020"},
#' multi-year labels such as \code{"1950--1960"},
#' and intervals that are open on the left,
#' such as \code{"<2000"}.
#'
#' As discussed in \code{\link{date_to_cohort_year}},
#' single-year labels such as \code{"2000"} are ambiguous.
#' Correctly aligning single-year and multi-year cohorts
#' requires knowing which month cohorts start on,
#' and whether single-year cohorts are labelled according
#' to the calendar year at the start of the cohort or the end.
#' \code{format_cohort_multi} assumes that
#' cohorts start in January, and that single-year cohorts
#' are labelled by calendar year at the start. These
#' settings can be changed using \code{month_start}
#' and \code{label_year_start}.
#'
#' \code{open_first} defaults to \code{TRUE}
#' if a value for \code{break_min} is supplied,
#' or if any intervals in \code{x} are open,
#' and to \code{FALSE} otherwise.
#' 
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_cohort_multi}
#' also contain \code{NA}.
#'
#' @inheritParams format_cohort_year
#' @param width The length, in whole years, of the cohorts.
#' Defaults to 5.
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
#' cohort labels are 
#' \code{\link{format_cohort_year}},
#' \code{\link{format_cohort_custom}},
#' \code{\link{format_cohort_quarter}},
#' and \code{\link{format_cohort_month}}.
#'
#' \code{\link{date_to_cohort_multi}} creates
#' multi-year cohorts from dates.
#'
#' \code{\link{make_labels_cohort}} describes the rules
#' for constructing labels for cohorts.
#'
#' @examples
#' format_cohort_multi(x = c("2000", "2005--2010", NA, "1995--1999"))
#'
#' ## contains open interval
#' format_cohort_multi(x = c("2000", "2005--2010", NA, "<1995"))
#'
#' ## changing the interpretation of the labels results in the
#' ## reclassification of cohort "2000"
#' format_cohort_multi(x = c("2000", "2005--2010", NA, "1995--1999"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#' 
#' ## 'break_min' is higher than the minimum of 'x'
#' format_cohort_multi(x = c("2000", "2005--2010", NA, "1995--1999"),
#'                     break_min = 2005)
#'
#' ## 'break_min' is lower then the minimum of 'x'
#' format_cohort_multi(x = c("2000", "2005--2010", NA, "1995--1999"),
#'                     break_min = 1990)
#'
#' ## 'break_min' supplied, but 'open_first' is FALSE
#' format_cohort_multi(x = c("2000", "2005--2010", NA, "1995--1999"),
#'                     break_min = 1990,
#'                     open_first = FALSE)
#'
#' ## non-default value for 'width'
#' format_cohort_multi(x = c("2000", "2005--2010", NA, "1995--1999"),
#'                     width = 10)
#'
#' ## non-default value for 'origin', to shift labels by one year
#' format_cohort_multi(x = c("2000", "2005--2010", NA, "1995--1999"),
#'                     width = 10,
#'                     origin = 2001)
#' @export 
format_cohort_multi <- function(x,
                                width = 5,
                                origin = 2000,
                                break_min = NULL,
                                open_first = NULL,
                                month_start = "Jan",
                                label_year_start = TRUE) {
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if (has_break_min) {
        break_min <- demcheck::err_tdy_integer_scalar(x = break_min,
                                                      name = "break_min")
        origin <- break_min
        message(gettextf("setting '%s' to %d",
                         "origin", origin))
    }
    else {
        origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                                   name = "origin")
    }
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    }
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
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## parse the labels
    parsed <- parse_integers_intervals(x = labels_x,
                                       name = "x",
                                       month_start = month_start,
                                       label_year_start = label_year_start)
    low <- parsed$low # integer
    up <- parsed$up   # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_min_x <- parsed$break_min # integer
    break_max_x <- parsed$break_max # integer
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", labels_x[[i_open_last]]),
             call. = FALSE)
    }
    ## deal with situations where 'open_first' was supplied
    if (has_open_first) {
        ## if 'open_first' is TRUE, and 'break_min' is supplied, and there are open intervals,
        ## check that the open intervals all start at or below 'break_min'
        if (open_first && has_break_min && any(is_open_first)) {
            is_too_high <- is_open_first & (up > break_min)
            i_too_high <- match(TRUE, is_too_high, nomatch = 0L)
            if (i_too_high > 0L) {
                stop(gettextf("'%s' has open interval [\"%s\"] that ends above '%s' [%d]",
                              "x", labels_x[[i_too_high]], "break_min", break_min),
                     call. = FALSE)
            }
        }
        ## if 'open_first' is FALSE, check that there are no open intervals
        if (!open_first) {
            i_is_open <- match(TRUE, is_open_first, nomatch = 0L)
            if (i_is_open > 0L)
                stop(gettextf("'%s' is %s but '%s' has open interval [\"%s\"]",
                              "open_first", "FALSE", "x", labels_x[[i_is_open]]),
                     call. = FALSE)
        }
    }
    else { ## where 'open_first' not supplied, assign a default value
        open_first <- any(is_open_first) || has_break_min
        message(gettextf("setting '%s' to %s",
                         "open_first", open_first))
    }
    ## make 'break_min', 'break_max'
    if (!has_break_min) {
        remainder_min <- (break_min_x - origin) %% width
        if (remainder_min == 0L)
            break_min <- break_min_x
        else {
            if (any(is_open_first))
                break_min <- break_min_x - remainder_min + width
            else
                break_min <- break_min_x - remainder_min
        }
        message(gettextf("setting '%s' to %d",
                         "break_min", break_min))
    }
    remainder_max <- (break_max_x - origin) %% width
    if (remainder_max == 0L)
        break_max <- break_max_x
    else
        break_max <- break_max_x - remainder + width
    ## make 'breaks'
    breaks <- seq.int(from = break_min,
                      to = break_max,
                      by = width)
    ## check that all intervals fall within implied breaks
    i_interval <- make_i_breaks(low = low,
                                up = up,
                                breaks = breaks,
                                open_first = open_first,
                                open_last = FALSE)
    is_multiple_intervals <- i_interval == -1L
    i_multiple_intervals <- match(TRUE, is_multiple_intervals, nomatch = 0L)
    if (i_multiple_intervals > 0L)
        stop(gettextf("label \"%\" from '%s' intersects two or more intervals formed using '%s' = %d and '%s' = %d",
                      labels_x[[i_multiple_intervals ]], "x", "origin", origin, "width", width),
             call. = FALSE)
    ## make labels
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_period_custom(breaks = breaks,
                                            open_first = open_first,
                                            include_na = include_na)
    ## assign new labels to x and return
    ans <- labels_new[i_interval]
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Put cohort labels into the format required
#' for customised cohorts
#'
#' Given a vector of cohort labels, create a
#' \code{\link[base]{factor}}
#' that contains levels for all cohorts
#' defined by \code{breaks}. \code{format_cohort_custom}
#' is the most flexible
#' of the \code{format_cohort} functions
#' in that the cohorts can have any combination of widths,
#' though the widths must be defined in whole numbers of years.
#' 
#' If \code{open_first} is \code{TRUE}, then the earliest
#' cohort has no lower limit. (This is equivalent
#' to an open age group with no upper limit.)
#'
#' The elements of \code{x} must be
#' multi-year labels such as \code{"1950-1960"} and
#' \code{"2020-2025"}, or labels for intervals
#' that are open on the left, such as \code{"<2000"}
#' and \code{<"1960"}.
#'
#' \code{open_first} defaults to \code{TRUE}
#' if any of the intervals in \code{x} is open,
#' and to \code{FALSE} otherwise.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_cohort_custom}
#' also contain \code{NA}.
#'
#' @inheritParams format_cohort_year
#' @param breaks A vector of strictly increasing integer values.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' cohort labels are 
#' \code{\link{format_cohort_year}},
#' \code{\link{format_cohort_multi}},
#' \code{\link{format_cohort_quarter}},
#' and \code{\link{format_cohort_month}}.
#'
#' \code{\link{date_to_cohort_custom}} creates
#' customized cohorts from dates.
#'
#' \code{\link{make_labels_cohort}} describes the rules
#' for constructing labels for cohorts.
#' @examples
#' format_cohort_custom(x = c("2000-2001", "2005-2010", "1995-1999"),
#'                      breaks = c(1990, 2000, 2020))
#'
#' format_cohort_custom(x = c("2000-2001", "2005-2010", "1995-1999"),
#'                      breaks = c(1995, 2005, 2010, 2020))
#' @export 
format_cohort_custom <- function(x,
                                 breaks,
                                 open_first = NULL,
                                 month_start = "Jan",
                                 label_year_start = TRUE) {
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments
    breaks <- demcheck::err_tdy_breaks_integer_cohort(breaks = breaks,
                                                      open_first = open_first)
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    }
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
    ## extract 'break_min' and 'break_max' from breaks
    break_min <- breaks[[1L]]
    break_max <- breaks[[n_break]]
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## classify labels_x, raising error for any invalid ones
    parsed <- parse_integers_intervals(x = labels_x,
                                       name = "x",
                                       month_start = month_start,
                                       label_year_start = label_year_start)
    low <- parsed$low # integer
    up <- parsed$up   # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", labels_x[[i_open_last]]),
             call. = FALSE)
    }
    ## deal with situations where 'open_first' was supplied
    if (has_open_first) {
        ## if 'open_first' is TRUE and there are open intervals,
        ## check that the open intervals all start at or below 'break_min'
        if (open_first && any(is_open_first)) {
            is_too_high <- is_open_first & (up > break_min)
            i_too_high <- match(TRUE, is_too_high, nomatch = 0L)
            if (i_too_high > 0L) {
                stop(gettextf("'%s' has open interval [\"%s\"] that ends above minimum for '%s' [%d]",
                              "x", labels_x[[i_too_high]], "breaks", break_min),
                     call. = FALSE)
            }
        }
        ## if 'open_first' is FALSE, check that there are no open intervals
        if (!open_first) {
            i_is_open <- match(TRUE, is_open_first, nomatch = 0L)
            if (i_is_open > 0L)
                stop(gettextf("'%s' is %s but '%s' has open interval [\"%s\"]",
                              "open_first", "FALSE", "x", labels_x[[i_is_open]]),
                     call. = FALSE)
        }
    }
    else { ## where 'open_first' not supplied, assign a default value
        open_first <- any(is_open_first)
        message(gettextf("setting '%s' to %s",
                         "open_first", open_first))
    }
    ## check times within bounds set by breaks
    if (!open_first) {
        is_too_low <- !is.na(low) & (low < breaks[[1L]])
        i_too_low <- match(TRUE, is_too_low, nomatch = 0L)
        if (i_too_low > 0L)
            stop(gettextf("interval '%s' starts below lowest value of '%s' [%d]",
                          labels_x[[i_too_low]], "breaks", breaks[[1L]]),
                 call. = FALSE)
    }
    is_too_high <- !is.na(up) & (up > breaks[[n_break]])
    i_too_high <- match(TRUE, is_too_high, nomatch = 0L)
    if (i_too_high > 0L)
        stop(gettextf("interval '%s' ends above highest value of '%s' [%d]",
                      labels_x[[i_too_high]], "breaks", breaks[[n_break]]),
             call. = FALSE)
    ## check that intervals do not cross boundaries set by breaks
    i_interval <- make_i_breaks(low = low,
                                up = up,
                                breaks = breaks,
                                open_first = open_first,
                                open_last = FALSE)
    is_multiple_intervals <- i_interval == -1L
    i_multiple_intervals <- match(TRUE, is_multiple_intervals, nomatch = 0L)
    if (i_multiple_intervals > 0L)
        stop(gettextf("label \"%\" from '%s' intersects two or more intervals formed using '%s'",
                      labels_x[[i_multiple_intervals]], "x", "breaks"),
             call. = FALSE)
    ## make labels
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_period_custom(breaks = breaks,
                                            open_first = open_first,
                                            include_na = include_na)
    ## assign new labels to x and return
    ans <- labels_new[i_interval]
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Put cohort labels into the format required
#' for quarter (three-month) cohorts
#'
#' Given a vector of cohort labels, create a
#' \code{\link[base]{factor}} where the levels
#' contain a complete set of cohorts. 
#' If \code{open_first} is \code{TRUE}, then the earliest
#' cohort has no lower limit. (This is equivalent
#' to an open age group with no upper limit.)
#'
#' The elements of \code{x} must be quarter
#' labels, such as \code{"2001 Q2"} or \code{"2055 Q1"}, including
#' labels for cohorts that are open on the left,
#' such as \code{"<2000 Q3"}
#' or \code{<"1960 Q4"}.
#'
#' \code{open_first} defaults to \code{TRUE}
#' if a value for \code{break_min} is supplied,
#' or if any intervals in \code{x} is open,
#' and to \code{FALSE} otherwise.
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
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_cohort_quarter}
#' also contain \code{NA}.
#'
#' @inheritParams format_cohort_year
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' cohort labels are 
#' \code{\link{format_cohort_year}},
#' \code{\link{format_cohort_multi}},
#' \code{\link{format_cohort_custom}},
#' and \code{\link{format_cohort_month}}.
#'
#' \code{\link{date_to_cohort_quarter}} creates
#' quarter cohorts from dates.
#'
#' @examples
#' format_cohort_quarter(x = c("2000 Q4", "2005 Q1", NA, "2004 Q3"))
#'
#' ## 'open_first' defaults to TRUE, since 'x'
#' ## contains an open interval
#' format_cohort_quarter(x = c("<2000 Q4", "2005 Q1", NA, "2004 Q3"))
#'
#' ## 'open_first' defaults to TRUE, since
#' ## a value for 'break_min' is supplied
#' format_cohort_quarter(x = c("2000 Q4", "2005 Q1", NA, "2004 Q3"),
#'                       break_min = "2004 Q1")
#'
#' ## 'break_min' specified, and 'open_first' set to FALSE
#' format_cohort_quarter(x = c("2005 Q1", NA, "2004 Q3"),
#'                       break_min = "2004 Q1",
#'                       open_first = FALSE)
#' @export 
format_cohort_quarter <- function(x,
                                  break_min = NULL,
                                  open_first = NULL) {
    format_cohort_month_quarter_year(x = x,
                                     break_min = break_min,
                                     open_first = open_first,
                                     parse_fun = parse_quarters,
                                     label_fun = make_labels_cohort_quarter)
}


## HAS_TESTS
#' Put cohort labels into the format required
#' for one-month cohorts
#'
#' Given a vector of cohort labels, create a
#' \code{\link[base]{factor}} where the levels
#' contain a complete set of cohorts.
#' If \code{open_first} is \code{TRUE}, then the earliest
#' cohort has no lower limit. (This is equivalent
#' to an open age group with no upper limit.)
#'
#' The elements of \code{x} must be month
#' labels, such as \code{"2001 Jan"} or \code{"2055 Sep"}, including
#' labels for cohorts that are open on the left,
#' such as \code{"<2000 Mar"}
#' or \code{<"1960 Dec"}.
#'
#' \code{open_first} defaults to \code{TRUE}
#' if a value for \code{break_min} is supplied,
#' or if any intervals in \code{x} is open,
#' and to \code{FALSE} otherwise.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_cohort_month}
#' also contain \code{NA}.
#'
#' @inheritParams format_cohort_year
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' cohort labels are 
#' \code{\link{format_cohort_year}},
#' \code{\link{format_cohort_multi}},
#' \code{\link{format_cohort_custom}},
#' and \code{\link{format_cohort_quarter}}.
#'
#' \code{\link{date_to_cohort_month}} creates
#' month cohorts from dates.
#'
#' @examples
#' format_cohort_month(x = c("2003 Dec", "2005 Jan", NA, "2004 Sep"))
#'
#' ## 'open_first' defaults to TRUE, since 'x'
#' ## contains an open interval
#' format_cohort_month(x = c("<2003 Dec", "2005 Jan", NA, "2004 Sep"))
#'
#' ## 'open_first' defaults to TRUE, since
#' ## a value for 'break_min' is supplied
#' format_cohort_month(x = c("2003 Dec", "2005 Jan", NA, "2004 Sep"),
#'                     break_min = "2004 Jun")
#'
#' ## 'break_min' specified, and 'open_first' set to FALSE
#' format_cohort_month(x = c("2005 Jan", NA, "2004 May"),
#'                       break_min = "2004 Jan",
#'                       open_first = FALSE)
#' @export 
format_cohort_month <- function(x,
                                break_min = NULL,
                                open_first = NULL) {
    format_cohort_month_quarter_year(x = x,
                                     break_min = break_min,
                                     open_first = open_first,
                                     parse_fun = parse_months,
                                     label_fun = make_labels_cohort_month)
}
