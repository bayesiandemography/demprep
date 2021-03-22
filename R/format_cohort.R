## HAS_TESTS
#' Put cohort labels into the format required
#' for single-year cohorts
#'
#' Given a vector of cohort labels, create a
#' \code{\link[base]{factor}} where the levels
#' contain a complete set of cohorts. If a value for
#' \code{break_min} is supplied, then the complete set
#' runs from the cohort specified by \code{break_min}
#' through to the youngest cohort in the data. If
#' no value for \code{break_min} is supplied, then
#' the set stats with the oldest cohort in the data.
#' 
#' If \code{open_first} is \code{TRUE}, then the oldest
#' cohort has no lower limit.
#' \code{open_first} defaults to \code{TRUE}
#' if a value for \code{break_min} is supplied,
#' and to \code{FALSE} otherwise.
#'
#' The elements of \code{x} must all be single years,
#' eg \code{"2001"} or \code{"2055"}, or cohorts
#' that are open on the left, eg \code{"<2000"}
#' or \code{<"1960"}.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_cohort_year}
#' also contain \code{NA}.
#'
#' @param x A vector of cohort labels.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
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
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 1998)
#'
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 2001)
#'
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 1998,
#'                    open_first = FALSE)
#'
#' ## if no value for 'break_min' is supplied, and one or
#' ## more of the labels refers to an open cohort,
#' ## then the levels start with the most
#' ## recent open cohort
#' format_cohort_year(x = c("2000", "2005", "<2002", "2004", "<2001"))
#'
#' @export 
format_cohort_year <- function(x,
                               break_min = NULL,
                               open_first = NULL) {
    ## regexp patterns
    p_single <- "^-?[0-9]+$"
    p_open <- "^<-?[0-9]+$"
    ## check arguments
    break_min <- demcheck::err_tdy_integer_scalar(x = break_min,
                                                  name = "break_min",
                                                  null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")



    
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
        stop(gettextf("\"%s\" is not a valid label for a single-year cohort",
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
                                                open_first = open_first,
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
#' Put cohort labels into the format required
#' for multi-year cohorts
#'
#' Given a vector of cohort labels, create a
#' \code{\link[base]{factor}}  containing
#' levels for the earliest and lastest cohorts
#' in \code{x}, and for all cohorts in between.
#' For instance, if the earliest cohort in \code{x}
#' is \code{"1990-1995"}, and the latest is \code{"2005-2010"},
#' then \code{format_cohort_multi} creates a factor
#' with levels \code{"1990-1995"}, \code{"1995-2000"},
#' \code{"2000-2005"},and \code{"2005-2010"}.
#'
#' The elements of \code{x} must be labels with
#' the same format as \code{"2001-2011"} or \code{"2055-2070"},
#' ie a start year and an end year, separated by a dash.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_cohort_year}
#' also contain \code{NA}.
#'
#' @inheritParams format_cohort_year
#' @param width The length, in whole years, of the cohorts.
#' Defaults to 5.
#' @param origin An integer. Defaults to 2000.
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
#' format_cohort_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"))
#'
#' format_cohort_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"),
#'                     width = 10)
#'
#'
#'
#' format_cohort_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"),
#'                     width = 10,
#'                     origin = 2001)
#' @export 
format_cohort_multi <- function(x,
                                width = 5,
                                origin = 2000,
                                break_min = NULL,
                                open_first = NULL) {
    ## regexp patterns
    p_single <- "^-?[0-9]+$"
    p_low_up <- "^(-?[0-9]+)-(-?[0-9]+)$"
    p_open <- "^<-?[0-9]+$"
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width",
                                                       null_ok = TRUE)
    if (has_break_min) {
        break_min <- demcheck::err_tdy_integer_scalar(x = break_min,
                                                      name = "break_min",
                                                      null_ok = TRUE)
        origin <- NULL
    }
    else {
        origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                                   name = "origin")
    }
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    }
    else
        open_first <- has_break_min
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
    is_low_up <- grepl(p_low_up, labels_old)
    is_valid <- is_na | is_single | is_low_up
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label for a multi-year cohort",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    year_low <- rep(NA_integer_, times = length(labels_old))
    year_up <- year_low
    year_low[is_single] <- as.integer(labels_old[is_single])
    year_up[is_single] <- year_low[is_single] + 1L
    year_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    year_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up]))
    year_up[is_open] <- as.integer(sub("^<", "", labels_old[is_single]))
    demcheck::err_interval_diff_ge_one(int_low = year_low,
                                       int_up = year_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## if 'break_min' is supplied, make sure no
    ## intervals less than 'break_min'
    if (!is.null(break_min)) {
        demcheck::err_interval_label_ge_break_min(labels = labels_old,
                                                  int_low = age_low,
                                                  break_min = break_min)
    }
    ## if 'open_last' is FALSE, check that there
    ## are no open age groups
    if (!open_last) {
        demcheck::err_no_open_age(labels_old)
    }
    ## if 'open_last' is FALSE, and 'break_max' is supplied,
    ## make sure that all intervals less than 'break_max'
    if (!open_last && !is.null(break_max)) {
        demcheck::err_interval_label_le_break_max(labels = labels_old,
                                                  int_up = age_up,
                                                  break_max = break_max)
    }





    ## make breaks
    breaks <- make_breaks_label_to_integer_year(int_low = year_low,
                                                int_up = year_up,
                                                width = width,
                                                origin = origin,
                                                is_open = any(is_open),
                                                break_min = break_min,
                                                break_max = NULL,
                                                open_first = open_first,
                                                open_last = FALSE)
    ## make labels for these breaks
    include_na <- any(is_na)
    labels_new <- make_labels_grouped_int_endpoints(breaks = breaks,
                                                    open_first = open_first,
                                                    open_last = FALSE,
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


## ## HAS_TESTS
## #' Put cohort labels into the format required
## #' for customized cohorts
## #'
## #' Given a vector of cohort labels, create a
## #' \code{\link[base]{factor}}
## #' that contains levels for all cohorts
## #' defined by \code{breaks}. \code{format_cohort_custom}
## #' is the most flexible
## #' of the \code{format_cohort} functions
## #' in that the cohorts can have any combination of widths,
## #' though the widths must be defined in whole numbers of years.
## #'
## #' The elements of \code{x} must all be labels with a
## #' start year and an end year, separated by a dash,
## #' eg \code{"2001-2011"} or \code{"2055-2070"}.
## #'
## #' If \code{x} contains \code{NA}, then the
## #' levels of the factor created by \code{format_cohort_year}
## #' also contain \code{NA}.
## #'
## #' @param x A vector of character labels.
## #' @param breaks A vector of strictly increasing integer values.
## #'
## #' @return A factor with the same length as
## #' \code{x}.
## #'
## #' @seealso Other functions for reformating
## #' cohort labels are 
## #' \code{\link{format_cohort_year}},
## #' \code{\link{format_cohort_multi}},
## #' \code{\link{format_cohort_quarter}},
## #' and \code{\link{format_cohort_month}}.
## #'
## #' \code{\link{date_to_cohort_custom}} creates
## #' customized cohorts from dates.
## #'
## #' \code{\link{make_labels_cohort}} describes the rules
## #' for constructing labels for cohorts.
## #' @examples
## #' format_cohort_custom(x = c("2000-2001", "2005-2010", "1995-1999"),
## #'                      breaks = c(1990, 2000, 2020))
## #'
## #' format_cohort_custom(x = c("2000-2001", "2005-2010", "1995-1999"),
## #'                      breaks = c(1995, 2005, 2010, 2020))
## #' @export 
## format_cohort_custom <- function(x,
##                                  breaks) {
##     ## regexp patterns
##     p_low_up <- "^([0-9]+)-([0-9]+)$"
##     ## check arguments
##     breaks <- demcheck::err_tdy_breaks_integer_cohort(breaks = breaks)
##     ## deal with "empty" case where 'breaks' has length 0
##     n_break <- length(breaks)
##     n_x <- length(x)
##     if (n_break == 0L) {
##         if (n_x > 0L) {
##             stop(gettextf("'%s' has length %d",
##                           "breaks", 0L),
##                  call. = FALSE)
##         }
##         else {
##             ans <- factor()
##             return(ans)
##         }
##     }
##     ## put unique values in 'labels_old' vector
##     labels_old <- unique(x)
##     ## classify labels_old, raising error for any invalid ones
##     is_na <- is.na(labels_old)
##     is_low_up <- grepl(p_low_up, labels_old)
##     is_valid <- is_na | is_low_up
##     i_invalid <- match(FALSE, is_valid, nomatch = 0L)
##     if (i_invalid > 0L)
##         stop(gettextf("\"%s\" is not a valid label for a custom cohort",
##                       labels_old[[i_invalid]]),
##              call. = FALSE)
##     ## extract lower and upper ages
##     year_low <- rep(NA_integer_, times = length(labels_old))
##     year_up <- year_low
##     year_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
##     year_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up])) + 1L
##     demcheck::err_interval_diff_gt_one(int_low = year_low,
##                                        int_up = year_up,
##                                        is_low_up = is_low_up,
##                                        labels = labels_old)
##     ## check years within bounds set by breaks
##     break_min <- breaks[[1L]]
##     break_max <- breaks[[n_break]]
##     demcheck::err_interval_label_ge_break_min(labels = labels_old,
##                                               int_low = year_low,
##                                               break_min = break_min)
##     demcheck::err_interval_label_le_break_max(labels = labels_old,
##                                               int_up = year_up,
##                                               break_max = break_max)
##     ## make labels for these breaks
##     include_na <- any(is_na)
##     labels_new <- make_labels_grouped_int_endpoints(breaks = breaks,
##                                                     open_first = FALSE,
##                                                     open_last = FALSE,
##                                                     include_na = include_na)
##     ## assign new labels to x
##     i_label_old <- match(x, labels_old)
##     year <- year_low[i_label_old]
##     i_intervals_new <- findInterval(x = year,
##                                     vec = breaks)
##     ans <- labels_new[i_intervals_new]
##     ## return result
##     ans <- factor(x = ans,
##                   levels = labels_new,
##                   exclude = NULL)
##     ans
## }


## ## HAS_TESTS
## #' Put cohort labels into the format required
## #' for quarter (three-month) cohorts
## #'
## #' Given a vector of cohort labels, create a
## #' \code{\link[base]{factor}} that contains
## #' levels for the earliest and latest cohorts in \code{x},
## #' and for all cohorts in between.
## #' For instance, if the earliest cohort in \code{x}
## #' is \code{"1990 Q1"}, and the latest is \code{"2010 Q4"},
## #' then \code{format_cohort_quarter} creates a factor
## #' with levels \code{"1990 Q1"}, \code{"1990 Q2"}, \dots,
## #' \code{"2010 Q3"}, \code{"2010 Q4"}.
## #'
## #' Quarters are defined as follows:
## #' \tabular{lll}{
## #'   \strong{Quarter} \tab \strong{Start} \tab \strong{End} \cr
## #'   Q1 \tab 1 January \tab 31 March \cr
## #'   Q2 \tab 1 April \tab 30 June \cr
## #'   Q3 \tab 1 July \tab 30 September \cr
## #'   Q4 \tab 1 October \tab 31 December
## #' }
## #'
## #' The elements of \code{x} must all be single quarters,
## #' eg \code{"2001 Q2"} or \code{"2055 Q1"}.
## #'
## #' If \code{x} contains \code{NA}, then the
## #' levels of the factor created by \code{format_cohort_quarter}
## #' also contain \code{NA}.
## #'
## #' @param x A vector of cohort labels.
## #'
## #' @return A factor with the same length as
## #' \code{x}.
## #'
## #' @seealso Other functions for reformating
## #' cohort labels are 
## #' \code{\link{format_cohort_year}},
## #' \code{\link{format_cohort_multi}},
## #' \code{\link{format_cohort_custom}},
## #' and \code{\link{format_cohort_month}}.
## #'
## #' \code{\link{date_to_cohort_quarter}} creates
## #' quarter cohorts from dates.
## #'
## #' @examples
## #' format_cohort_quarter(x = c("2000 Q4", "2005 Q1", NA, "2004 Q3"))
## #' @export 
## format_cohort_quarter <- function(x) {
##     ## regexp patterns
##     p_single <- "^[0-9]+ Q[1-4]$"
##     ## deal with "empty" cases where 'x'
##     ## has length 0 or is all NA
##     if (length(x) == 0L) {
##         ans <- factor()
##         return(ans)
##     }
##     if (all(is.na(x))) {
##         ans <- factor(x,
##                       levels = NA_character_,
##                       exclude = NULL)
##         return(ans)
##     }
##     ## put unique values in 'labels_old' vector
##     labels_old <- unique(x)
##     ## classify labels_old, raising error for any invalid ones
##     is_na <- is.na(labels_old)
##     is_single <- grepl(p_single, labels_old)
##     is_valid <- is_na | is_single
##     i_invalid <- match(FALSE, is_valid, nomatch = 0L)
##     if (i_invalid > 0L)
##         stop(gettextf("\"%s\" is not a valid label for a quarter cohort",
##                       labels_old[[i_invalid]]),
##              call. = FALSE)
##     ## convert labels to dates
##     date <- date_start_quarter(labels_old)
##     ## make breaks
##     breaks <- make_breaks_date_to_date_quarter(date = date,
##                                                break_min = NULL)
##     ## make labels for these breaks
##     include_na <- any(is_na)
##     n <- length(breaks)
##     break_min <- breaks[[1L]]
##     break_max <- breaks[[n]]
##     labels_new <- make_labels_cohort_quarter(break_min = break_min,
##                                              break_max = break_max,
##                                              include_na = include_na)
##     ## assign new labels to x
##     i_label_old <- match(x, labels_old)
##     table_old_new <- match(labels_old, labels_new)
##     i_label_new <- table_old_new[i_label_old]
##     ans <- labels_new[i_label_new]
##     ## return result
##     ans <- factor(x = ans,
##                   levels = labels_new,
##                   exclude = NULL)
##     ans
## }


## ## HAS_TESTS
## #' Put cohort labels into the format required
## #' for one-month cohorts
## #'
## #' Given a vector of cohort labels, create a
## #' \code{\link[base]{factor}} that contains
## #' levels for the earliest and latest cohorts in \code{x},
## #' and for all cohorts in between.
## #' For instance, if the earliest cohort in \code{x}
## #' is \code{"1990 Jan"}, and the latest is \code{"2010 Dec"},
## #' then \code{format_cohort_month} creates a factor
## #' with levels \code{"1990 Jan"}, \code{"1990 Feb"}, \dots,
## #' \code{"2010 Nov"}, \code{"2010 Dec"}.
## #'
## #' The elements of \code{x} must all be single months,
## #' eg \code{"2001 Feb"} or \code{"2055 Mar"}.
## #'
## #' If \code{x} contains \code{NA}, then the
## #' levels of the factor created by \code{format_cohort_month}
## #' also contain \code{NA}.
## #'
## #' @param x A vector of cohort labels.
## #'
## #' @return A factor with the same length as
## #' \code{x}.
## #'
## #' @seealso Other functions for reformating
## #' cohort labels are 
## #' \code{\link{format_cohort_year}},
## #' \code{\link{format_cohort_multi}},
## #' \code{\link{format_cohort_custom}},
## #' and \code{\link{format_cohort_quarter}}.
## #'
## #' \code{\link{date_to_cohort_month}} creates
## #' month cohorts from dates.
## #'
## #' @examples
## #' format_cohort_month(x = c("2000 Sep", "2005 May", NA, "2004 Jan"))
## #' @export 
## format_cohort_month <- function(x) {
##     ## regexp patterns
##     p_single <- paste(sprintf("^[0-9]+ %s$", month.abb), collapse = "|")
##     ## deal with "empty" cases where 'x'
##     ## has length 0 or is all NA
##     if (length(x) == 0L) {
##         ans <- factor()
##         return(ans)
##     }
##     if (all(is.na(x))) {
##         ans <- factor(x,
##                       levels = NA_character_,
##                       exclude = NULL)
##         return(ans)
##     }
##     ## put unique values in 'labels_old' vector
##     labels_old <- unique(x)
##     ## classify labels_old, raising error for any invalid ones
##     is_na <- is.na(labels_old)
##     is_single <- grepl(p_single, labels_old)
##     is_valid <- is_na | is_single
##     i_invalid <- match(FALSE, is_valid, nomatch = 0L)
##     if (i_invalid > 0L)
##         stop(gettextf("\"%s\" is not a valid label for a month cohort",
##                       labels_old[[i_invalid]]),
##              call. = FALSE)
##     ## convert labels to dates
##     date <- date_start_month(labels_old)
##     ## make breaks
##     breaks <- make_breaks_date_to_date_month(date = date,
##                                              break_min = NULL)
##     ## make labels for these breaks
##     include_na <- any(is_na)
##     n <- length(breaks)
##     break_min <- breaks[[1L]]
##     break_max <- breaks[[n]]
##     labels_new <- make_labels_cohort_month(break_min = break_min,
##                                            break_max = break_max,
##                                            include_na = include_na)
##     ## assign new labels to x
##     i_label_old <- match(x, labels_old)
##     table_old_new <- match(labels_old, labels_new)
##     i_label_new <- table_old_new[i_label_old]
##     ans <- labels_new[i_label_new]
##     ## return result
##     ans <- factor(x = ans,
##                   levels = labels_new,
##                   exclude = NULL)
##     ans
## }
