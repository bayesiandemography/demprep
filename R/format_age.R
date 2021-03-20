
## HAS_TESTS
#' Put age group labels into the format required
#' for single-year age groups
#'
#' Given a vector of age group labels, create a factor
#' that contains levels for all ages between \code{break_min}
#' and \code{break_max}, and that may contain an open age group
#' (ie an age group with no upper limit.)
#' If, for instance, \code{break_min} is \code{0},
#' \code{break_max} is \code{100}, and \code{open_last} is
#' \code{TRUE} (the defaults), then \code{format_age_year}
#' creates a factor with levels \code{"0"}, \code{"1"},
#' \dots, \code{"99"}, \code{"100+"}. Even when an age
#' group between \code{break_min} and \code{break_max}
#' is not included in the data \code{x}, \code{factor_age_year}
#' still creates a level for it.
#' 
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{format_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' All age groups in \code{x} must be single-year age groups,
#' except for age groups above \code{break_max}.
#'
#' @param x A vector of age group labels.
#' @param break_min An integer or \code{NULL}.
#' Defaults to 0.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 100.
#' @param open_last Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' age group labels are 
#' \code{\link{format_age_multi}},
#' \code{\link{format_age_lifetab}},
#' \code{\link{format_age_births}},
#' \code{\link{format_age_custom}},
#' \code{\link{format_age_quarter}},
#' and \code{\link{format_age_month}}.
#'
#' \code{\link{date_to_age_year}} creates
#' one-year age groups from dates.
#'
#' \code{\link{make_labels_age}} describes the rules
#' for constructing labels for age groups.
#' @examples
#' format_age_year(x = c("10", "3", "100+", "77"))
#'
#' ## multi-year age groups are allowed,
#' ## provided they are above 'break_max'
#' format_age_year(x = c("15", "30-39", "35+"),
#'                 break_max = 30)
#'
#' ## allow 'break_min' and 'break_max' to be
#' ## determined by the data
#' format_age_year(x = c("22", "7", "30"),
#'                 break_min = NULL,
#'                 break_max = NULL)
#'
#' ## allow 'break_max' to be determined
#' ## by the data, which includes an
#' ## open age group
#' format_age_year(x = c("17", "10+"),
#'                 break_max = NULL)
#'
#' ## oldest age group is closed
#' format_age_year(x = c("10", "3", "77"),
#'                 open_last = FALSE)
#' @export 
format_age_year <- function(x,
                            break_min = 0,
                            break_max = 100,
                            open_last = TRUE) {
    format_age_multi(x = x,
                     width = 1L,
                     break_min = break_min,
                     break_max = break_max,
                     open_last = open_last)
}


## HAS_TESTS
#' Put age group labels into the format required
#' for multi-year age groups
#'
#' Given a vector of age group labels, create a factor
#' that contains levels for all ages between \code{break_min}
#' and \code{break_max}, and that may contain an open age group
#' (ie an age group with no upper limit.)
#' Apart from the open age group, the age groups all
#' have the width specified by \code{width}.
#' If, for instance, \code{break_min} is \code{0},
#' \code{break_max} is \code{100}, \code{width} is \code{5},
#' and \code{open_last} is \code{TRUE} (the defaults),
#' then \code{format_age_multi} creates
#' a factor with levels \code{"0-4"}, \code{"5-9"},
#' \dots, \code{"95-99"}, \code{"100+"}. Even when an age
#' group between \code{break_min} and \code{break_max}
#' is not included in the data \code{x}, \code{factor_age_year}
#' still creates a level for it.
#' 
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{format_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' All age groups in \code{x} must fall within the intervals
#' determined by \code{break_min} and \code{width},
#' except for age groups above \code{break_max}.
#'
#' @inheritParams format_age_year
#' @param width The width in years of the age intervals.
#' A positive integer. Defaults to 5.
#'
#' @return A factor with the same length as
#' \code{x}.
#'
#' @seealso Other functions for reformating
#' age group labels are 
#' \code{\link{format_age_year}},
#' \code{\link{format_age_lifetab}},
#' \code{\link{format_age_births}},
#' \code{\link{format_age_custom}},
#' \code{\link{format_age_quarter}},
#' and \code{\link{format_age_month}}.
#'
#' \code{\link{date_to_age_multi}} creates
#' multi-year age groups from dates.
#'
#' \code{\link{make_labels_age}} describes the rules
#' for constructing labels for age groups.
#' @examples
#' format_age_multi(x = c("10-14", "3", "100+", "77-79"))
#' 
#' format_age_multi(x = c("10-14", "3", "100+", "77-79"),
#'                  width = 10)
#'
#' ## age groups wider than the 'width' argument
#' ## are allowed, provided they are above 'break_max'
#' format_age_multi(x = c("15", "30-39", "45-59"),
#'                  width = 10,
#'                  break_max = 40)
#'
#' ## allow 'break_min' and 'break_max' to be
#' ## determined by the data
#' format_age_multi(x = c("22", "7", "30"),
#'                  break_min = NULL,
#'                  break_max = NULL)
#'
#' ## allow 'break_max' to be determined
#' ## by the data, which includes an
#' ## open age group
#' format_age_multi(x = c("17", "10+"),
#'                  break_max = NULL)
#'
#' ## oldest age group is closed
#' format_age_multi(x = c("10", "3", "77"),
#'                  open_last = FALSE)
#' @export 
format_age_multi <- function(x,
                             width = 5, 
                             break_min = 0,
                             break_max = 100,
                             open_last = TRUE) {
    ## regexp patterns
    p_single <- "^[0-9]+$"
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    p_open <- "[0-9]+\\+$"
    ## check arguments
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width",
                                                       null_ok = TRUE)
    break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                               name = "break_min",
                                                               null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
        demcheck::err_difference_divisible(x1 = break_max,
                                           x2 = break_min,
                                           y = width,
                                           name1 = "break_max",
                                           name2 = "break_min",
                                           name_y = "width")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with "empty" case where 'x' has no non-NA values
    ## and 'break_min' or 'break_max' is missing
    ## (so cannot construct levels)
    n <- length(x)
    all_empty <- (n == 0L) || all(is.na(x))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (all_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n)
        ans <- factor(ans,
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
        stop(gettextf("\"%s\" is not a valid label for an age group",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    age_low <- rep(NA_integer_, times = length(labels_old))
    age_up <- age_low
    age_low[is_single] <- as.integer(labels_old[is_single])
    age_up[is_single] <- age_low[is_single] + 1L
    age_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    age_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up])) + 1L
    age_low[is_open] <- as.integer(sub("\\+", "", labels_old[is_open]))
    demcheck::err_interval_diff_gt_one(int_low = age_low,
                                       int_up = age_up,
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
    breaks <- make_breaks_label_to_integer_year(int_low = age_low,
                                                int_up = age_up,
                                                labels = labels,
                                                width = width,
                                                origin = 0L,
                                                is_open = is_open,
                                                break_min = break_min,
                                                break_max = break_max,
                                                open_last = open_last)
    ## make labels for these breaks
    include_na <- any(is_na)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = open_last,
                                  include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    age <- age_low[i_label_old]
    i_intervals_new <- findInterval(x = age,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## ## HAS_TESTS
#' Put age group labels into the format
#' required for abridged life table
#'
#' Given a vector of age group labels, create a factor that contains
#' levels for all ages between \code{0} and \code{break_max},
#' plus an open age group (ie an age group with no upper limit.)
#' These age groups are the ones typically used in
#' "abridged" (ie not single-year) life tables: \code{"0"},
#' \code{"1-4"}, \code{"5-9"}, \code{"10-14"}, and so on up to the
#' highest age group, which is always open.
#'
#' \code{break_max} is used to specify
#' the oldest age group.
#' If \code{break_max} is \code{NULL}, the oldest
#' age group is derived from the data.
#'
#' All age groups in \code{x} must fall within the intervals
#' \code{"0"}, \code{"1-4"}, \code{"5-9"}, \dots,
#' except for age groups above \code{break_max}.
#'
#' @inheritParams format_age_year
#'
#' @return A factor with the same length as \code{x}.
#'
#' @seealso Other functions for reformating
#' age group labels are 
#' \code{\link{format_age_year}},
#' \code{\link{format_age_multi}},
#' \code{\link{format_age_births}},
#' \code{\link{format_age_custom}},
#' \code{\link{format_age_quarter}},
#' and \code{\link{format_age_month}}.
#'
#' \code{\link{date_to_age_lifetab}} creates
#' lifetable age groups from dates.
#'
#' \code{\link{make_labels_age}} describes the rules
#' for constructing labels for age groups.
#' 
#' @examples
#' format_age_lifetab(x = c("100+", "14", "1-4"))
#'
#' ## set oldest age group to 50+
#' format_age_lifetab(x = c("100+", "14", "1-4"),
#'                    break_max = 80)
#' @export
format_age_lifetab <- function(x, break_max = 100) {
    ## regexp patterns
    p_single <- "^[0-9]+$"
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    p_open <- "[0-9]+\\+$"
    ## check arguments
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_max)) {
        demcheck::err_multiple_of_n(x = break_max,
                                    name = "break_max",
                                    n = 5L,
                                    null_ok = FALSE)
    }
    ## deal with "empty" case where 'x' has no non-NA values
    ## and 'break_max' is missing (so cannot construct levels)
    n <- length(x)
    all_empty <- (n == 0) || all(is.na(x))
    is_unbounded <- is.null(break_max)
    if (all_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n)
        ans <- factor(ans,
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
        stop(gettextf("\"%s\" is not a valid label for an age group",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    age_low <- rep(NA_integer_, times = length(labels_old))
    age_up <- age_low
    age_low[is_single] <- as.integer(labels_old[is_single])
    age_up[is_single] <- age_low[is_single] + 1L
    age_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    age_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up])) + 1L
    age_low[is_open] <- as.integer(sub("\\+", "", labels_old[is_open]))
    demcheck::err_interval_diff_gt_one(int_low = age_low,
                                       int_up = age_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## make breaks
    breaks <- make_breaks_label_to_integer_lifetab(age_low = age_low,
                                                   age_up = age_up,
                                                   labels = labels,
                                                   is_open = is_open,
                                                   break_max = break_max)
    ## make labels for breaks
    include_na <- any(is_na)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = TRUE,
                                  include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    age <- age_low[i_label_old]
    i_intervals_new <- findInterval(x = age,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Put age groups labels into format
#' required for tabulating births
#'
#' Given a vector of age group labels, create a factor that contains
#' levels for all ages between \code{break_min} and \code{break_max}.
#'
#' \code{break_min} and \code{break_max} specify
#' the range of ages over which reproduction
#' is assumed to occur. If, for instance,
#' \code{break_min} is \code{15} and \code{break_max}
#' is \code{50}, all births are assumed to
#' occur to women aged 15 to 49 (inclusive).
#'
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{date_to_age_births}
#' finds the narrowest range that accommodates the values
#' in \code{x}.
#'
#' Datasets sometimes contain a few births to parents
#' younger than the assumed minimum age of reproduction,
#' or births to parents older than the assumed maximum age
#' of reproduction. Demographers often recode such births,
#' so that ones to unexpectedly young parents are
#' treated as occurring just above the minimum age
#' for reproduction, and ones to unexpectedly old parents
#' are treated as occurring just below the maximum
#' age for reproduction. This recoding can be justified
#' on the grounds that some of the original ages may have
#' been misreported, but it also alleviates any problems
#' with tabulations having small counts at extreme ages.
#' Recoding of parents' ages outside the expected range
#' is controlled by parameters \code{recode_up}
#' and \code{recode_down}. The default
#' is for no recoding to occur.
#'
#' @param x A vector of age group labels. The
#' age of the parent at the time of the birth of
#' the child.
#' @param break_min An integer or \code{NULL}.
#' Defaults to 15.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 50.
#' @param width The width in years of the age intervals.
#' A positive integer defaulting to 5.
#' @param recode_up If \code{TRUE}, births to parents
#' aged less than \code{break_min} are treated as occurring to
#' people in the lowest repoductive age group.
#' @param recode_down If \code{TRUE}, births to parents
#' aged \code{break_max} or more are treated as
#' occurring to people in the highest reproductive
#' age group.
#'
#' @return A factor with the same length as \code{x}.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{format_age_year}},
#' \code{\link{format_age_multi}},
#' \code{\link{format_age_lifetab}},
#' \code{\link{format_age_custom}},
#' \code{\link{format_age_quarter}},
#' and \code{\link{format_age_month}}.
#'
#' \code{\link{date_to_age_births}} creates
#' reproductive age groups from dates.
#'
#' \code{\link{make_labels_age}} describes the rules
#' for constructing labels for age groups.
#'
#' @examples
#' format_age_births(x = c("20-24", "37", NA, "32", "21-24"))
#' 
#' format_age_births(x = c("20-24", "37", "32", "21-24"),
#'                   width = 10,
#'                   break_min = 20)
#'
#' format_age_births(x = c("20", "37", "15"),
#'                   width = 1,
#'                   break_max = 45)
#'
#' ## allow youngest and oldest age groups to be
#' ## determined by the data
#' format_age_births(x = c("21", "33", "22-24"),
#'                   break_min = NULL,
#'                   break_max = NULL)
#'
#' ## recode ages outside the expected range
#' format_age_births(x = c("22", "13", "54"),
#'                   recode_up = TRUE,
#'                   recode_down = TRUE)
#' @export
format_age_births <- function(x,
                              break_min = 15,
                              break_max = 50,
                              width = 5,
                              recode_up = FALSE,
                              recode_down = FALSE) {
    ## regexp patterns
    p_single <- "^[0-9]+$"
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    ## check arguments
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width",
                                                       null_ok = TRUE)
    break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                               name = "break_min",
                                                               null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
        demcheck::err_difference_divisible(x1 = break_max,
                                           x2 = break_min,
                                           y = width,
                                           name1 = "break_max",
                                           name2 = "break_min",
                                           name_y = "width")
    }
    demcheck::err_is_logical_flag(x = recode_up,
                                  name = "recode_up")
    demcheck::err_is_logical_flag(x = recode_down,
                                  name = "recode_down")
    ## deal with "empty" case where 'x' has no non-NA values
    ## and 'break_min' or 'break_max' is missing
    ## (so cannot construct levels)
    n <- length(x)
    all_empty <- (n == 0L) || all(is.na(x))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (all_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n)
        ans <- factor(ans,
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
        stop(gettextf("\"%s\" is not a valid label for an age group",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    age_low <- rep(NA_integer_, times = length(labels_old))
    age_up <- age_low
    age_low[is_single] <- as.integer(labels_old[is_single])
    age_up[is_single] <- age_low[is_single] + 1L
    age_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    age_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up])) + 1L
    demcheck::err_interval_diff_gt_one(int_low = age_low,
                                       int_up = age_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## check that ages lie within limits implied by 'break_min' and 'break_max'
    if (!is.null(break_min)) {
        is_lt_min <- age_low < break_min
        i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
        if (i_lt_min > 0L) {
            if (recode_up) {
                age_low[is_lt_min] <- break_min
                age_up[is_lt_min] <- pmax(age_up[is_lt_min], age_low[is_lt_min] + 1L)
            }
            else {
                stop(gettextf("age group \"%s\" less than 'break_min' [%d] and 'recode_up' is FALSE",
                              labels_old[[i_lt_min]],
                              break_min),
                     call. = FALSE)
            }
        }
    }
    if (!is.null(break_max)) {
        is_gt_max <- age_up > break_max
        i_gt_max <- match(TRUE, is_gt_max, nomatch = 0L)
        if (i_gt_max > 0L) {
            if (recode_down) {
                age_up[is_gt_max] <- break_max
                age_low[is_gt_max] <- pmin(age_low[is_gt_max], age_up[is_gt_max] - 1L)
            }
            else {
                stop(gettextf("age group \"%s\" greater than 'break_max' [%d] and 'recode_down' is FALSE",
                              labels_old[[i_gt_max]],
                              break_max),
                     call. = FALSE)
            }
        }
    }
    ## make breaks
    breaks <- make_breaks_label_to_integer_births(age_low = age_low,
                                                  age_up = age_up,
                                                  labels = labels,
                                                  width = width,
                                                  break_min = break_min,
                                                  break_max = break_max)
    ## make labels for breaks
    include_na <- any(is_na)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = FALSE,
                                  include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    age <- age_low[i_label_old]
    i_intervals_new <- findInterval(x = age,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Put age group labels into format required
#' for customized age groups
#'
#' Given a vector of age group labels, create a factor
#' that contains levels for all age groups
#' defined by \code{breaks}, including, possibly,
#' an open age group.\code{format_age_custom} is the most flexible
#' of the \code{format_age} functions
#' in that the age groups can have any combination of widths,
#' though the widths must be defined in whole numbers of years.
#'
#' \code{breaks} is used to specify the points at which
#' each age group starts and finishes. If 
#' \code{open_last} is \code{TRUE}, and \code{b} is
#' the last value for \code{breaks}, then the oldest
#' age group is \code{[b, Inf)} years. 
#' If \code{open_last} is \code{FALSE}, \code{a} is the
#' second-to-last value for \code{breaks} and \code{b}
#' is the last value, then the oldest age
#' group is \code{[a, b)} years.
#'
#' @inheritParams format_age_year
#' @param breaks A vector of strictly increasing integer values.
#'
#' @return A factor with length equal to \code{x}.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{format_age_year}},
#' \code{\link{format_age_multi}},
#' \code{\link{format_age_lifetab}},
#' \code{\link{format_age_births}},
#' \code{\link{format_age_quarter}},
#' and \code{\link{format_age_month}}.
#'
#' \code{\link{date_to_age_custom}} creates
#' customized age groups from dates.
#'
#' \code{\link{make_labels_age}} describes the rules
#' for constructing labels for age groups.
#'
#' @examples
#' format_age_custom(x = c("90+", "19-40", "22", NA),
#'                   breaks = c(0, 15, 60))
#' format_age_custom(x = c("50-59", "19-40", "31"),
#'                   breaks = c(15, 45, 60),
#'                   open_last = FALSE)
#' @export
format_age_custom <- function(x,
                              breaks = NULL,
                              open_last = TRUE) {
    ## regexp patterns
    p_single <- "^[0-9]+$"
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    p_open <- "[0-9]+\\+$"
    ## check arguments
    breaks <- demcheck::err_tdy_breaks_integer_age(breaks = breaks,
                                                   open_last = open_last)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
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
    is_single <- grepl(p_single, labels_old)
    is_low_up <- grepl(p_low_up, labels_old)
    is_open <- grepl(p_open, labels_old)
    is_valid <- is_na | is_single | is_low_up | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label for an age group",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    age_low <- rep(NA_integer_, times = length(labels_old))
    age_up <- age_low
    age_low[is_single] <- as.integer(labels_old[is_single])
    age_up[is_single] <- age_low[is_single] + 1L
    age_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
    age_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up])) + 1L
    age_low[is_open] <- as.integer(sub("\\+", "", labels_old[is_open]))
    demcheck::err_interval_diff_gt_one(int_low = age_low,
                                       int_up = age_up,
                                       is_low_up = is_low_up,
                                       labels = labels_old)
    ## check that ages lie within limits implied by 'breaks' and 'open_last'
    is_lt_min <- age_low < breaks[[1L]]
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        stop(gettextf("age group \"%s\" is below minimum value for '%s' [%d]",
                      labels_old[[i_lt_min]],
                      "breaks",
                      breaks[[1L]]),
             call. = FALSE)
    }
    if (!open_last) {
        is_gt_max <- age_up > breaks[[n_break]]
        i_gt_max <- match(TRUE, is_gt_max, nomatch = 0L)
        if (i_gt_max > 0L) {
            stop(gettextf("age group \"%s\" is above maximum value for '%s' [%d]",
                          labels_old[[i_gt_max]],
                          "breaks",
                          breaks[[n_break]]),
                 call. = FALSE)
        }
    }
    ## check that age groups do not cross breaks
    demcheck::err_intervals_inside_breaks(int_low = age_low,
                                          int_up = age_up,
                                          breaks = breaks,
                                          labels = labels_old)
    ## make labels for breaks
    include_na <- any(is_na)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = open_last,
                                  include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    age <- age_low[i_label_old]
    i_intervals_new <- findInterval(x = age,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Put age group labels into format required
#' for one-quarter age groups
#'
#' Given a vector of age group labels, create a factor
#' that contains levels for all ages between \code{break_min}
#' and \code{break_max}, and that may contain an open age group
#' (ie an age group with no upper limit.)
#' If, for instance, \code{break_min} is \code{0},
#' \code{break_max} is \code{400}, and \code{open_last} is
#' \code{TRUE} (the defaults), then \code{format_age_year}
#' creates a factor with levels \code{"0q"}, \code{"1q"},
#' \dots, \code{"399q"}, \code{"400q+"}. Even when an age
#' group between \code{break_min} and \code{break_max}
#' is not included in \code{x}, \code{factor_age_quarter}
#' still creates a level for it.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} quarters
#' old at their most recent birthday. For instance, a person
#' belongs to age group \code{"20q"} if that person had
#' their 5th birthday (= 20 quarters) two days ago.
#' 
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{format_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' All age groups in \code{x} must be single-quarter age groups,
#' except above \code{break_max}, where open age groups
#' are allowed.
#'
#' @inheritParams format_age_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 400.
#'
#' @return A factor with the same length as \code{x}.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{format_age_year}},
#' \code{\link{format_age_multi}},
#' \code{\link{format_age_lifetab}},
#' \code{\link{format_age_births}},
#' \code{\link{format_age_custom}},
#' and \code{\link{format_age_month}}.
#'
#' \code{\link{make_labels_age_quarter}} describes the rules
#' for constructing labels for quarter age groups.
#'
#' @examples
#' format_age_quarter(x = c("10q", "22q", "500q+"))
#'
#' ## specify highest age group
#' format_age_quarter(x = c("10q", "22q", "500q+"),
#'                    break_max = 48)
#'
#' ## let lowest age group be determined by the data
#' format_age_quarter(x = c("10q", "22q", "500q+"),
#'                    break_min = NULL,
#'                    break_max = 48)
#'
#' ## make final age group closed
#' format_age_quarter(x = c("10q", "22q"),
#'                    break_min = NULL,
#'                    break_max = 48,
#'                    open_last = FALSE)
#' @export
format_age_quarter <- function(x, 
                               break_min = 0,
                               break_max = 400,
                               open_last = TRUE) {
    ## regexp patterns
    p_single <- "^[0-9]+q$"
    p_open <- "[0-9]+q\\+$"
    ## check arguments
    break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                               name = "break_min",
                                                               null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with "empty" case where 'x' has no non-NA values
    ## and 'break_min' or 'break_max' is missing
    ## (so cannot construct levels)
    n <- length(x)
    all_empty <- (n == 0L) || all(is.na(x))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (all_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n)
        ans <- factor(ans,
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
        stop(gettextf("\"%s\" is not a valid label for an age group",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    age_low <- rep(NA_integer_, times = length(labels_old))
    age_up <- age_low
    age_low[is_single] <- as.integer(sub("q", "", labels_old[is_single]))
    age_up[is_single] <- age_low[is_single] + 1L
    age_low[is_open] <- as.integer(sub("q\\+", "", labels_old[is_open]))
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
    breaks <- make_breaks_label_to_integer_month_quarter(age_low = age_low,
                                                         age_up = age_up,
                                                         labels = labels,
                                                         is_open = is_open,
                                                         break_min = break_min,
                                                         break_max = break_max,
                                                         open_last = open_last)
    ## make labels for these breaks
    include_na <- any(is_na)
    n_break <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n_break]]
    labels_new <- make_labels_age_quarter(break_min = break_min,
                                          break_max = break_max,
                                          open_last = open_last,
                                          include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    age <- age_low[i_label_old]
    i_intervals_new <- findInterval(x = age,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Put age group labels into format required
#' for one-month age groups
#'
#' Given a vector of age group labels, create a factor
#' that contains levels for all ages between \code{break_min}
#' and \code{break_max}, and that may contain an open age group
#' (ie an age group with no upper limit.)
#' If, for instance, \code{break_min} is \code{0},
#' \code{break_max} is \code{1200}, and \code{open_last} is
#' \code{TRUE} (the defaults), then \code{format_age_year}
#' creates a factor with levels \code{"0m"}, \code{"1m"},
#' \dots, \code{"1199m"}, \code{"1200m+"}. Even when an age
#' group between \code{break_min} and \code{break_max}
#' is not included in \code{x}, \code{factor_age_month}
#' still creates a level for it.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} months
#' old at their most recent birthday.
#' For instance, a person belongs to age
#' group \code{"60m"} if that person had their
#' 5th birthday (= 60 months) two days ago.
#'
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{format_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' All age groups in \code{x} must be single-quarter age groups,
#' except above \code{break_max}, where open age groups
#' are allowed.
#'
#' @inheritParams format_age_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 1200.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The length of the return value equals the length
#' of \code{date} or the length of \code{dob}, whichever
#' is greater.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{format_age_year}},
#' \code{\link{format_age_multi}},
#' \code{\link{format_age_lifetab}},
#' \code{\link{format_age_births}},
#' \code{\link{format_age_custom}},
#' \code{\link{format_age_quarter}}.
#'
#' \code{\link{make_labels_age_month}} describes the rules
#' for constructing labels for month age groups.
#'
#' @examples
#' format_age_month(x = c("3m", "12m", "1400m+"))
#'
#' ## specify highest age group
#' format_age_month(x = c("3m", "12m", "1400m+"),
#'                  break_max = 24)
#'
#' ## let lowest age group be determined by the data
#' format_age_month(x = c("3m", "12m", "1400m+"),
#'                  break_min = NULL,
#'                  break_max = 24)
#'
#' ## make final age group closed
#' format_age_month(x = c("3m", "12m"),
#'                  break_min = NULL,
#'                  break_max = 24,
#'                  open_last = FALSE)
#' @export
format_age_month <- function(x,
                             break_min = 0,
                             break_max = 1200,
                             open_last = TRUE) {
    ## regexp patterns
    p_single <- "^[0-9]+m$"
    p_open <- "[0-9]+m\\+$"
    ## check arguments
    break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                               name = "break_min",
                                                               null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with "empty" case where 'x' has no non-NA values
    ## and 'break_min' or 'break_max' is missing
    ## (so cannot construct levels)
    n <- length(x)
    all_empty <- (n == 0L) || all(is.na(x))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (all_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n)
        ans <- factor(ans,
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
        stop(gettextf("\"%s\" is not a valid label for an age group",
                      labels_old[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper ages
    age_low <- rep(NA_integer_, times = length(labels_old))
    age_up <- age_low
    age_low[is_single] <- as.integer(sub("m", "", labels_old[is_single]))
    age_up[is_single] <- age_low[is_single] + 1L
    age_low[is_open] <- as.integer(sub("m\\+", "", labels_old[is_open]))
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
    breaks <- make_breaks_label_to_integer_month_quarter(age_low = age_low,
                                                         age_up = age_up,
                                                         labels = labels,
                                                         is_open = is_open,
                                                         break_min = break_min,
                                                         break_max = break_max,
                                                         open_last = open_last)
    ## make labels for these breaks
    include_na <- any(is_na)
    n_break <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n_break]]
    labels_new <- make_labels_age_month(break_min = break_min,
                                        break_max = break_max,
                                        open_last = open_last,
                                        include_na = include_na)
    ## assign new labels to x
    i_label_old <- match(x, labels_old)
    age <- age_low[i_label_old]
    i_intervals_new <- findInterval(x = age,
                                    vec = breaks)
    ans <- labels_new[i_intervals_new]
    ## return result
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


