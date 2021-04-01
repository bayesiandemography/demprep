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
#' The elements of \code{x} are typically single-year
#' labels, such as \code{"2001"} or \code{"2055"}, or
#' labels for cohorts
#' that are open on the left, such as \code{"<2000"}
#' or \code{<"1960"}. However, when \code{open_first}
#' is \code{TRUE}, \code{x} can also include
#' multi-year labels such as \code{"1950-1960"},
#' provided that the intervals are less
#' than \code{break_min} (and hence are absorbed
#' into the open age group).
#'
#' \code{open_first} defaults to \code{TRUE}
#' if a value for \code{break_min} is supplied,
#' or if any intervals in \code{x} is open,
#' and to \code{FALSE} otherwise.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_cohort_year}
#' also contain \code{NA}.
#'
#' @param x A vector of cohort labels.
#' @param break_min The lower limit of the cohort,
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
#' ## specify 'break_min', which makes also
#' ## 'open_first' default to TRUE
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 1998)
#'
#' ## set 'break_min' above minimum value of 'x'
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 2001)
#'
#' ## multi-year labels are allowed if they are earlier than 'break_min'
#' format_cohort_year(x = c("1950-1980", "2005", NA, "2004"),
#'                    break_min = 2001)
#'
#' ## set 'open_first' to FALSE 
#' format_cohort_year(x = c("2000", "2005", NA, "2004"),
#'                    break_min = 1998,
#'                    open_first = FALSE)
#'
#' ## if no value for 'break_min' is supplied, and one or
#' ## more of the labels refers to an open cohort,
#' ## then the levels start with the most
#' ## recent open cohort
#' format_cohort_year(x = c("2000", "2005", "<2002", "2004", "<2001"))
#' @export 
format_cohort_year <- function(x,
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
    break_min <- demcheck::err_tdy_integer_scalar(x = break_min,
                                                  name = "break_min",
                                                  null_ok = TRUE)
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    }
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
    is_open <- grepl(p_open, labels_old)
    is_valid <- is_na | is_single | is_low_up | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## determine value for 'open_first', if not supplied
    if (!has_open_first)
        open_first <- has_break_min || any(is_open)
    ## extract lower and upper years
    year_low <- rep(NA_integer_, times = length(labels_old))
    year_up <- year_low
    year_low[is_single] <- as.integer(labels_old[is_single])
    year_up[is_single] <- year_low[is_single] + 1L
    year_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    year_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up]))
    year_up[is_open] <- as.integer(sub("^<", "", labels_old[is_open]))
    demcheck::err_interval_diff_ge_one(int_low = year_low,
                                       int_up = year_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## if 'open_first' is TRUE and 'break_min' is supplied, check that
    ## all open intervals start at or below 'break_min'
    if (open_first && !is.null(break_min)) {
        demcheck::err_open_left_le_break_min(labels = labels_old,
                                             int_up = year_up,
                                             is_open = is_open,
                                             break_min = break_min)
    }                                              
    ## if 'open_first' is FALSE, check that there
    ## are no open age groups
    if (!open_first) {
        demcheck::err_no_open_cohort(labels_old)
    }
    ## if 'open_first' is FALSE, and 'break_min' is supplied,
    ## make sure that all intervals greater than or equal to 'break_min'
    if (!open_first && has_break_min) {
        demcheck::err_interval_label_ge_break_min(labels = labels_old,
                                                  int_low = year_low,
                                                  break_min = break_min)
    }
    ## make breaks
    breaks <- make_breaks_label_to_integer_year(int_low = year_low,
                                                int_up = year_up,
                                                width = 1L,
                                                origin = 2000L,
                                                is_open = is_open,
                                                break_min = break_min,
                                                break_max = NULL,
                                                has_break_min_arg = TRUE,
                                                has_break_max_arg = FALSE,
                                                open_first = open_first,
                                                open_last = FALSE)
    ## make labels for these breaks
    include_na <- any(is_na)
    labels_new <- make_labels_grouped_int_enumerations(breaks = breaks,
                                                       open_first = open_first,
                                                       open_last = FALSE,
                                                       include_na = include_na)
    ## assign new labels to x
    i_labels_old <- match(x, labels_old)
    year <- year_low[i_labels_old]
    i_labels_new <- findInterval(x = year,
                                 vec = breaks)
    if (open_first) {
        is_open_x <- x %in% labels_old[is_open]
        i_labels_new[is_open_x] <- 1L
        i_labels_new[!is_open_x] <- i_labels_new[!is_open_x] + 1L
    }
    ans <- labels_new[i_labels_new]
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
#' levels for the earliest and latest cohorts
#' in \code{x}, and for all cohorts in between.
#' For instance, if the earliest cohort in \code{x}
#' is \code{"1990-1995"}, and the latest is \code{"2005-2010"},
#' then \code{format_cohort_multi} creates a factor
#' with levels \code{"1990-1995"}, \code{"1995-2000"},
#' \code{"2000-2005"},and \code{"2005-2010"}.
#' All cohorts in the return value (with the possible
#' exception of the earliest) have the same length,
#' which is controlled by the \code{width} parameter.
#'
#' If \code{open_first} is \code{TRUE}, then the earliest
#' cohort has no lower limit. (This is equivalent
#' to an open age group with no upper limit.)
#' 
#' The elements of \code{x} must be multi-year
#' intervals such as \code{"1950-1960"},
#' \code{"2020-2025"}, or intervals that are
#' open on the left, such as \code{"<2000"}
#' or \code{<"1960"}.
#'
#' \code{open_first} defaults to \code{TRUE}
#' if a value for \code{break_min} is supplied,
#' or if any intervals in \code{x} is open,
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
#' ## 'break_min' is higher than the minimum of 'x'
#' format_cohort_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"),
#'                     break_min = 2005)
#'
#' ## 'break_min' is lower then the minimum of 'x'
#' format_cohort_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"),
#'                     break_min = 1990)
#'
#' ## 'break_min' supplied, but 'open_first' is FALSE
#' format_cohort_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"),
#'                     break_min = 1990,
#'                     open_first = FALSE)
#'
#' ## non-default value for 'width'
#' format_cohort_multi(x = c("2000-2001", "2005-2010", NA, "1995-1999"),
#'                     width = 10)
#'
#' ## non-default value for 'origin', to shift labels by one year
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
                                                      null_ok = FALSE)
        origin <- break_min
    }
    else {
        origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                                   name = "origin")
    }
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    }
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
    is_open <- grepl(p_open, labels_old)
    is_valid <- is_na | is_low_up | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## determine value for 'open_first', if not supplied
    if (!has_open_first)
        open_first <- has_break_min || any(is_open)
    ## extract lower and upper ages
    year_low <- rep(NA_integer_, times = length(labels_old))
    year_up <- year_low
    year_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    year_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up]))
    year_up[is_open] <- as.integer(sub("^<", "", labels_old[is_open]))
    demcheck::err_interval_diff_ge_one(int_low = year_low,
                                       int_up = year_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## if 'open_first' is TRUE and 'break_min' is supplied, check that
    ## all open intervals start at or below 'break_min'
    if (open_first && !is.null(break_min)) {
        demcheck::err_open_left_le_break_min(labels = labels_old,
                                             int_up = year_up,
                                             is_open = is_open,
                                             break_min = break_min)
    }                                              
    ## if 'open_first' is FALSE, check that there
    ## are no open age groups
    if (!open_first) {
        demcheck::err_no_open_cohort(labels_old)
    }
    ## if 'open_first' is FALSE, and 'break_min' is supplied,
    ## make sure that all intervals greater than or equal to 'break_min'
    if (!open_first && has_break_min) {
        demcheck::err_interval_label_ge_break_min(labels = labels_old,
                                                  int_low = year_low,
                                                  break_min = break_min)
    }
    ## make breaks
    breaks <- make_breaks_label_to_integer_year(int_low = year_low,
                                                int_up = year_up,
                                                width = width,
                                                origin = origin,
                                                is_open = any(is_open),
                                                break_min = break_min,
                                                break_max = NULL,
                                                has_break_min_arg = TRUE,
                                                has_break_max_arg = FALSE,
                                                open_first = open_first,
                                                open_last = FALSE)
    ## make labels for these breaks
    include_na <- any(is_na)
    labels_new <- make_labels_grouped_int_endpoints(breaks = breaks,
                                                    open_first = open_first,
                                                    open_last = FALSE,
                                                    include_na = include_na)
    ## assign new labels to x
    i_labels_old <- match(x, labels_old)
    year <- year_low[i_labels_old]
    i_labels_new <- findInterval(x = year,
                                 vec = breaks)
    if (open_first) {
        is_open_x <- x %in% labels_old[is_open]
        i_labels_new[is_open_x] <- 1L
        i_labels_new[!is_open_x] <- i_labels_new[!is_open_x] + 1L
    }
    ans <- labels_new[i_labels_new]
    ## return result
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
                                 open_first = NULL) {
    ## regexp patterns
    p_low_up <- "^(-?[0-9]+)-(-?[0-9]+)$"
    p_open <- "^<-?[0-9]+$"
    ## see if arguments supplied
    has_open_first <- !is.null(open_first)
    ## put unique values in 'labels_old' vector
    labels_old <- unique(x)
    ## check arguments
    if (has_open_first)
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    else
        open_first <- any(grepl(p_open, labels_old))
    breaks <- demcheck::err_tdy_breaks_integer_cohort(breaks = breaks,
                                                      open_first = open_first)
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
    is_open <- grepl(p_open, labels_old)
    is_valid <- is_na | is_low_up | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    year_low <- rep(NA_integer_, times = length(labels_old))
    year_up <- year_low
    year_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    year_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up]))
    year_up[is_open] <- as.integer(sub("^<", "", labels_old[is_open]))
    demcheck::err_interval_diff_ge_one(int_low = year_low,
                                       int_up = year_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## if 'open_first' is FALSE, check that there
    ## are no open age groups
    if (!open_first) {
        demcheck::err_no_open_cohort(labels_old)
    }
    ## check years within bounds set by breaks
    if (!open_first) {
        break_min <- breaks[[1L]]
        demcheck::err_interval_label_ge_break_min(labels = labels_old,
                                                  int_low = year_low,
                                                  break_min = break_min)
    }
    break_max <- breaks[[n_break]]
    demcheck::err_interval_label_le_break_max(labels = labels_old,
                                              int_up = year_up,
                                              break_max = break_max)
    ## make labels for these breaks
    include_na <- any(is_na)
    labels_new <- make_labels_grouped_int_endpoints(breaks = breaks,
                                                    open_first = open_first,
                                                    open_last = FALSE,
                                                    include_na = include_na)
    ## assign new labels to x
    i_labels_old <- match(x, labels_old)
    year <- year_low[i_labels_old]
    i_intervals_new <- findInterval(x = year,
                                    vec = breaks)
    if (open_first) {
        is_open_x <- x %in% labels_old[is_open]
        i_intervals_new[is_open_x] <- 1L
        i_intervals_new[!is_open_x] <- i_intervals_new[!is_open_x] + 1L
    }
    ans <- labels_new[i_intervals_new]
    ## return result
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
    ## regexp patterns
    p_single <- "^[0-9]+ Q[1-4]$"
    p_open <- "^<[0-9]+ Q[1-4]$"
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments
    if (has_break_min) {
        demcheck::err_is_string(break_min)
        if (!grepl(p_single, break_min))
            stop(gettextf("invalid value for '%s' : \"%s\"",
                          "break_min", break_min),
                 call. = FALSE)
        break_min <- date_start_quarter(break_min)
    }
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    }
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
    is_open <- grepl(p_open, labels_old)
    is_valid <- is_na | is_single | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## determine value for 'open_first', if not supplied
    if (!has_open_first)
        open_first <- has_break_min || any(is_open)
    ## extract lower and upper dates
    date_low <- rep(as.Date(NA), times = length(labels_old))
    date_up <- date_low
    date_low[is_single] <- date_start_quarter(labels_old[is_single])
    date_up[is_single] <- add_quarters(date = date_low[is_single],
                                       n = 1L)
    date_up[is_open] <- date_start_quarter(labels_old[is_open])
    ## if 'open_first' is TRUE and 'break_min' is supplied, check that
    ## all open intervals start at or below 'break_min'
    if (open_first && !is.null(break_min)) {
        demcheck::err_open_left_le_break_min(labels = labels_old,
                                             int_up = date_up,
                                             is_open = is_open,
                                             break_min = break_min)
    }                                              
    ## if 'open_first' is FALSE, check that there
    ## are no open age groups
    if (!open_first) {
        demcheck::err_no_open_cohort(labels_old)
    }
    ## if 'open_first' is FALSE, and 'break_min' is supplied,
    ## make sure that all intervals greater than or equal to 'break_min'
    if (!open_first && has_break_min) {
        demcheck::err_interval_label_ge_break_min(labels = labels_old,
                                                  int_low = date_low,
                                                  break_min = break_min)
    }
    ## make breaks
    breaks <- make_breaks_label_to_date_month_quarter(date_low = date_low,
                                                      date_up = date_up,
                                                      break_min = break_min,
                                                      has_break_min_arg = TRUE,
                                                      is_open = is_open,
                                                      unit = "quarter")
    ## make labels for these breaks
    include_na <- any(is_na)
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels_new <- make_labels_cohort_quarter(break_min = break_min,
                                             break_max = break_max,
                                             open_first = open_first,
                                             include_na = include_na)
    ## assign new labels to x
    i <- match(x, labels_new, nomatch = 0L) ## any non-matches must belong to open interval
    i[i == 0L] <- 1L
    ans <- labels_new[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
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
    ## regexp patterns
    p_single <- paste(sprintf("^[0-9]+ %s$", month.abb), collapse = "|")
    p_open <- paste(sprintf("^<[0-9]+ %s$", month.abb), collapse = "|")
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments
    if (has_break_min) {
        demcheck::err_is_string(break_min)
        if (!grepl(p_single, break_min))
            stop(gettextf("invalid value for '%s' : \"%s\"",
                          "break_min", break_min),
                 call. = FALSE)
        break_min <- date_start_month(break_min)
    }
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    }
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
    is_open <- grepl(p_open, labels_old)
    is_valid <- is_na | is_single | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## determine value for 'open_first', if not supplied
    if (!has_open_first)
        open_first <- has_break_min || any(is_open)
    ## extract lower and upper dates
    date_low <- rep(as.Date(NA), times = length(labels_old))
    date_up <- date_low
    date_low[is_single] <- date_start_month(labels_old[is_single])
    date_up[is_single] <- add_months(date = date_low[is_single],
                                     n = 1L)
    date_up[is_open] <- date_start_month(labels_old[is_open])
    ## if 'open_first' is TRUE and 'break_min' is supplied, check that
    ## all open intervals start at or below 'break_min'
    if (open_first && !is.null(break_min)) {
        demcheck::err_open_left_le_break_min(labels = labels_old,
                                             int_up = date_up,
                                             is_open = is_open,
                                             break_min = break_min)
    }                                              
    ## if 'open_first' is FALSE, check that there
    ## are no open age groups
    if (!open_first) {
        demcheck::err_no_open_cohort(labels_old)
    }
    ## if 'open_first' is FALSE, and 'break_min' is supplied,
    ## make sure that all intervals greater than or equal to 'break_min'
    if (!open_first && has_break_min) {
        demcheck::err_interval_label_ge_break_min(labels = labels_old,
                                                  int_low = date_low,
                                                  break_min = break_min)
    }
    ## make breaks
    breaks <- make_breaks_label_to_date_month_quarter(date_low = date_low,
                                                      date_up = date_up,
                                                      break_min = break_min,
                                                      has_break_min_arg = TRUE,
                                                      is_open = is_open,
                                                      unit = "month")
    ## make labels for these breaks
    include_na <- any(is_na)
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels_new <- make_labels_cohort_month(break_min = break_min,
                                           break_max = break_max,
                                           open_first = open_first,
                                           include_na = include_na)
    ## assign new labels to x
    i <- match(x, labels_new, nomatch = 0L) ## any non-matches must belong to open interval
    i[i == 0L] <- 1L
    ans <- labels_new[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}

