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
#' The elements of \code{x} must all be single years,
#' eg \code{"2001"} or \code{"2055"}.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_period_year}
#' also contain \code{NA}.
#'
#' @param x A vector of period labels.
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
#' \code{\link{make_labels_period}} describes the rules
#' for constructing labels for periods.
#'
#' @examples
#' format_period_year(x = c("2000", "2005", NA, "2004"))
#' @export 
format_period_year <- function(x) {
    ## regexp patterns
    p_single <- "^-?[0-9]+$"
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
    ## put unique values in 'labels_old' vector
    labels_old <- unique(x)
    ## classify labels_old, raising error for any invalid ones
    is_na <- is.na(labels_old)
    is_single <- grepl(p_single, labels_old)
    is_valid <- is_na | is_single
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label for a single-year period",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    year_low <- rep(NA_integer_, times = length(labels_old))
    year_up <- year_low
    year_low[is_single] <- as.integer(labels_old[is_single])
    year_up[is_single] <- year_low[is_single] + 1L
    ## make breaks
    breaks <- make_breaks_label_to_integer_year(int_low = year_low,
                                                int_up = year_up,
                                                width = 1L,
                                                origin = 2000L,
                                                is_open = FALSE,
                                                break_min = NULL,
                                                break_max = NULL,
                                                open_first = FALSE,
                                                open_last = FALSE)
    ## make labels for these breaks
    include_na <- any(is_na)
    n <- length(breaks)
    int_min <- breaks[[1L]]
    int_max <- breaks[[n]] - 1L
    labels_new <- make_labels_integers(int_min = int_min,
                                       int_max = int_max,
                                       include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    i_intervals_new <- findInterval(x = year_low,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Put period labels into the format required
#' for multi-year periods
#'
#' Given a vector of period labels, create a
#' \code{\link[base]{factor}}  containing
#' levels for the earliest and lastest periods
#' in \code{x}, and for all periods in between.
#' For instance, if the earliest period in \code{x}
#' is \code{"1990-1995"}, and the latest is \code{"2005-2010"},
#' then \code{format_period_multi} creates a factor
#' with levels \code{"1990-1995"}, \code{"1995-2000"},
#' \code{"2000-2005"},and \code{"2005-2010"}.
#'
#' The elements of \code{x} must be labels with
#' the same format as \code{"2001-2011"} or \code{"2055-2070"},
#' ie a start year and an end year, separated by a dash.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_period_year}
#' also contain \code{NA}.
#'
#' @inheritParams format_period_year
#' @param width The length, in whole years, of the periods.
#' Defaults to 5.
#' @param origin An integer. Defaults to 2000.
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
#' \code{\link{date_to_period_multi}} creates
#' multi-year periods from dates.
#'
#' \code{\link{make_labels_period}} describes the rules
#' for constructing labels for periods.
#'
#' @examples
#' format_period_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"))
#'
#' format_period_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"),
#'                     width = 10)
#'
#' format_period_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"),
#'                     width = 10,
#'                     origin = 2001)
#' @export 
format_period_multi <- function(x,
                                width = 5, 
                                origin = 2000) {
    ## regexp patterns
    p_low_up <- "^(-?[0-9]+)-(-?[0-9]+)$"
    ## check arguments
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width",
                                                       null_ok = TRUE)
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
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
    ## put unique values in 'labels_old' vector
    labels_old <- unique(x)
    ## classify labels_old, raising error for any invalid ones
    is_na <- is.na(labels_old)
    is_low_up <- grepl(p_low_up, labels_old)
    is_valid <- is_na | is_low_up
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label for a multi-year period",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    year_low <- rep(NA_integer_, times = length(labels_old))
    year_up <- year_low
    year_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    year_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up]))
    demcheck::err_interval_diff_ge_one(int_low = year_low,
                                       int_up = year_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## make breaks
    breaks <- make_breaks_label_to_integer_year(int_low = year_low,
                                                int_up = year_up,
                                                width = width,
                                                origin = origin,
                                                is_open = FALSE,
                                                break_min = NULL,
                                                break_max = NULL,
                                                open_first = FALSE,
                                                open_last = FALSE)
    ## make labels for these breaks
    include_na <- any(is_na)
    labels_new <- make_labels_grouped_int_endpoints(breaks = breaks,
                                                    open_first = FALSE,
                                                    open_last = FALSE,
                                                    include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    i_intervals_new <- findInterval(x = year_low,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Put period labels into the format required
#' for customized periods
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
#' The elements of \code{x} must all be labels with a
#' start year and an end year, separated by a dash,
#' eg \code{"2001-2011"} or \code{"2055-2070"}.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_period_year}
#' also contain \code{NA}.
#'
#' @param x A vector of character labels.
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
#' \code{\link{date_to_period_custom}} creates
#' customized periods from dates.
#'
#' \code{\link{make_labels_period}} describes the rules
#' for constructing labels for periods.
#' @examples
#' format_period_custom(x = c("2000-2001", "2005-2010", "1995-1999"),
#'                      breaks = c(1990, 2000, 2020))
#'
#' format_period_custom(x = c("2000-2001", "2005-2010", "1995-1999"),
#'                      breaks = c(1995, 2005, 2010, 2020))
#' @export 
format_period_custom <- function(x,
                                 breaks) {
    ## regexp patterns
    p_low_up <- "^(-?[0-9]+)-(-?[0-9]+)$"
    ## check arguments
    breaks <- demcheck::err_tdy_breaks_integer_period(breaks = breaks)
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
    ## put unique values in 'labels_old' vector
    labels_old <- unique(x)
    ## classify labels_old, raising error for any invalid ones
    is_na <- is.na(labels_old)
    is_low_up <- grepl(p_low_up, labels_old)
    is_valid <- is_na | is_low_up
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label for a custom period",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    year_low <- rep(NA_integer_, times = length(labels_old))
    year_up <- year_low
    year_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    year_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up])) + 1L
    demcheck::err_interval_diff_gt_one(int_low = year_low,
                                       int_up = year_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## check years within bounds set by breaks
    break_min <- breaks[[1L]]
    break_max <- breaks[[n_break]]
    demcheck::err_interval_label_ge_break_min(labels = labels_old,
                                              int_low = year_low,
                                              break_min = break_min)
    demcheck::err_interval_label_le_break_max(labels = labels_old,
                                              int_up = year_up,
                                              break_max = break_max)
    ## make labels for these breaks
    include_na <- any(is_na)
    labels_new <- make_labels_grouped_int_endpoints(breaks = breaks,
                                                    open_first = FALSE,
                                                    open_last = FALSE,
                                                    include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    year <- year_low[i_label_old]
    i_intervals_new <- findInterval(x = year,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
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
    ## regexp patterns
    p_single <- "^-?[0-9]+ Q[1-4]$"
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
    ## put unique values in 'labels_old' vector
    labels_old <- unique(x)
    ## classify labels_old, raising error for any invalid ones
    is_na <- is.na(labels_old)
    is_single <- grepl(p_single, labels_old)
    is_valid <- is_na | is_single
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label for a quarter period",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## convert labels to dates
    date <- date_start_quarter(labels_old)
    ## make breaks
    breaks <- make_breaks_date_to_date_quarter(date = date,
                                               break_min = NULL)
    ## make labels for these breaks
    include_na <- any(is_na)
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels_new <- make_labels_period_quarter(break_min = break_min,
                                             break_max = break_max,
                                             include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    table_old_new <- match(labels_old, labels_new)
    i_label_new <- table_old_new[i_label_old]
    ans <- labels_new[i_label_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
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
    ## regexp patterns
    p_single <- paste(sprintf("^-?[0-9]+ %s$", month.abb), collapse = "|")
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
    ## put unique values in 'labels_old' vector
    labels_old <- unique(x)
    ## classify labels_old, raising error for any invalid ones
    is_na <- is.na(labels_old)
    is_single <- grepl(p_single, labels_old)
    is_valid <- is_na | is_single
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label for a month period",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## convert labels to dates
    date <- date_start_month(labels_old)
    ## make breaks
    breaks <- make_breaks_date_to_date_month(date = date,
                                             break_min = NULL)
    ## make labels for these breaks
    include_na <- any(is_na)
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels_new <- make_labels_period_month(break_min = break_min,
                                           break_max = break_max,
                                           include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    table_old_new <- match(labels_old, labels_new)
    i_label_new <- table_old_new[i_label_old]
    ans <- labels_new[i_label_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}
