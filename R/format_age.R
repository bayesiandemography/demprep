
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
#' is not included in \code{x}, \code{format_age_year}
#' still creates a level for it.
#' 
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{format_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' All age groups in \code{x} must be single-year age groups,
#' except for any open age groups.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_age_year}
#' also contain \code{NA}.
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
#' @examples
#' format_age_year(x = c("10", "3", "100+", "77"))
#'
#' ## allow 'break_min' and 'break_max' to be
#' ## determined by the data
#' format_age_year(x = c(22, 7, 30),
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
#' format_age_year(x = c(10, 3, 77),
#'                 open_last = FALSE)
#' @export 
format_age_year <- function(x,
                            break_min = 0,
                            break_max = 100,
                            open_last = TRUE) {
    format_age_month_quarter_year(x = x,
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
#' Apart from the open age group, the age groups
#' produced by \code{format_age_multi} all
#' have the width specified by \code{width}.
#' If, for instance, \code{break_min} is \code{0},
#' \code{break_max} is \code{100}, \code{width} is \code{5},
#' and \code{open_last} is \code{TRUE} (the defaults),
#' then \code{format_age_multi} creates
#' a factor with levels \code{"0-4"}, \code{"5-9"},
#' \dots, \code{"95-99"}, \code{"100+"}. Even when an age
#' group between \code{break_min} and \code{break_max}
#' is not included in \code{x}, \code{format_age_multi}
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
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_age_multi}
#' also contain \code{NA}.
#'
#' @inheritParams format_age_year
#' @param width The width in years of the age
#' groups to be created. A positive integer.
#' Defaults to 5.
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
#' \code{\link{date_to_age_year}} creates
#' ages from dates.
#'
#' @examples
#' format_age_multi(x = c(22, 11, 99, NA))
#' 
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
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_break_max <- !is.null(break_max)
    ## check arguments
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if (has_break_min)
        break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                                   name = "break_min")
    if (has_break_max)
        break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                               name = "break_max")
    if (has_break_min && has_break_max) {
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
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (is_unbounded) {
        if (length(x) == 0L) {
            ans <- factor()
            return(ans)
        }
        if (all(is.na(x))) {
            ans <- factor(x,
                          levels = unique(x),
                          exclude = NULL)
            return(ans)
        }
    }
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## parse labels, and extract information
    parsed <- parse_quantities(x = labels_x,
                               name = "x")
    low <- parsed$low # integer
    up <- parsed$up   # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_min_x <- parsed$break_min # integer
    break_max_x <- parsed$break_max # integer
    i_open_first <- match(TRUE, is_open_first, nomatch = 0L)
    if (i_open_first > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
                      "x", labels_x[[i_open_first]]),
             call. = FALSE)
    }
    ## if 'break_min' is supplied, make sure that all intervals start
    ## at or above 'break_min'
    if (has_break_min) {
        is_too_low_min <- low < break_min
        i_too_low_min <- match(TRUE, is_too_low_min, nomatch = 0L)
        if (i_too_low_min > 0L) {
            stop(gettextf("'%s' has interval [\"%s\"] that starts below '%s' [%d]",
                          "x", labels_x[[i_too_low_min]], "break_min", break_min),
                 call. = FALSE)
        }
    }
    ## if 'open_last' is TRUE, and 'break_max' is supplied, and there are open intervals,
    ## check that the open intervals all start at or above 'break_max'
    if (open_last && has_break_max && any(is_open_last)) {
        is_too_low_max <- is_open_last & (low < break_max)
        i_too_low_max <- match(TRUE, is_too_low_max, nomatch = 0L)
        if (i_too_low_max > 0L) {
            stop(gettextf("'%s' has open interval [\"%s\"] that starts below '%s' [%d]",
                          "x", labels_x[[i_too_low_max]], "break_max", break_max),
                 call. = FALSE)
        }
    }
    ## if 'open_last' is FALSE, check that there are no open intervals
    if (!open_last) {
        i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
        if (i_open_last > 0L)
            stop(gettextf("'%s' is %s but '%s' has open interval [\"%s\"]",
                          "open_last", "FALSE", "x", labels_x[[i_open_last]]),
                 call. = FALSE)
    }
    ## if 'open_last' is FALSE, and 'break_max' is supplied,
    ## make sure that all intervals less than 'break_max'
    if (!open_last && has_break_max) {
        is_too_high <- up > break_max
        i_too_high <- match(TRUE, is_too_high, nomatch = 0L)
        if (i_too_high > 0L) {
            stop(gettextf("'%s' has interval [\"%s\"] that ends above '%s' [%d]",
                          "x", labels_x[[i_too_high]], "break_max", break_max),
                 call. = FALSE)
        }
    }
    ## make 'break_min', 'break_max'
    if (!has_break_min) {
        break_min <- break_min_x
        message(gettextf("setting '%s' to %d",
                         "break_min", break_min))
    }
    if (!has_break_max) {
        remainder_max <- (break_max_x - break_min) %% width
        if (remainder_max == 0L)
            break_max <- break_max_x
        else
            break_max <- break_max_x - remainder_max + width
        message(gettextf("setting '%s' to %d",
                         "break_max", break_max))
    }
    ## make 'breaks'
    breaks <- seq.int(from = break_min,
                      to = break_max,
                      by = width)
    ## check that all intervals fall within implied breaks
    i_interval <- make_i_interval(low = low,
                                  up = up,
                                  breaks = breaks,
                                  open_first = FALSE,
                                  open_last = open_last)
    is_multiple_intervals <- i_interval == -1L
    i_multiple_intervals <- match(TRUE, is_multiple_intervals, nomatch = 0L)
    if (i_multiple_intervals > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that intersects two or more intervals formed using '%s = %d', '%s = %d', and '%s = %d'",
                      "x",
                      labels_x[[i_multiple_intervals]],
                      "break_min",
                      break_min,
                      "break_max",
                      break_max,
                      "width",
                      width),
             call. = FALSE)
    ## make labels for these breaks
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = open_last,
                                  include_na = include_na)
    ## return result
    ans <- labels_new[i_interval][match(x, labels_x)]
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
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_age_lifetab}
#' also contain \code{NA}.
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
#' \code{\link{date_to_age_year}} calculates
#' ages from dates.
#'
#' @examples
#' format_age_lifetab(x = c("100+", "14", "1-4"))
#'
#' ## set oldest age group to 50+
#' format_age_lifetab(x = c("100+", "14", "1-4"),
#'                    break_max = 80)
#' @export
format_age_lifetab <- function(x, break_max = 100) {
    ## see if arguments supplied
    has_break_max <- !is.null(break_max)
    ## check arguments
    if (has_break_max) {
        break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max")
        demcheck::err_multiple_of_n(x = break_max,
                                    name = "break_max",
                                    n = 5L)
    }
    ## deal with "empty" case where 'x' has no non-NA values
    ## and 'break_max' is missing (so cannot construct levels)
    is_unbounded <- !has_break_max
    if (is_unbounded) {
        if (length(x) == 0L) {
            ans <- factor()
            return(ans)
        }
        if (all(is.na(x))) {
            ans <- factor(x,
                          levels = unique(x),
                          exclude = NULL)
            return(ans)
        }
    }
    ## put unique values in 'labels_old' vector
    labels_x <- unique(x)
    parsed <- parse_quantities(x = labels_x,
                               name = "x")
    low <- parsed$low # integer
    up <- parsed$up   # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_max_x <- parsed$break_max # integer
    i_open_first <- match(TRUE, is_open_first, nomatch = 0L)
    if (i_open_first > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
                      "x", labels_x[[i_open_first]]),
             call. = FALSE)
    }
    ## if 'break_max' is supplied, check that
    ## all open intervals start at or above 'break_max'
    if (has_break_max && any(is_open_last)) {
        is_too_low_max <- is_open_last & (low < break_max)
        i_too_low_max <- match(TRUE, is_too_low_max, nomatch = 0L)
        if (i_too_low_max > 0L) {
            stop(gettextf("'%s' has open interval [\"%s\"] that starts below '%s' [%d]",
                          "x", labels_x[[i_too_low_max]], "break_max", break_max),
                 call. = FALSE)
        }
    }
    ## make 'break_max'
    if (!has_break_max) {
        remainder_max <- break_max_x %% 5L
        if (remainder_max == 0L)
            break_max <- break_max_x
        else
            break_max <- break_max_x - remainder_max + 5L
        message(gettextf("setting '%s' to %d",
                         "break_max", break_max))
    }
    ## make breaks
    breaks <- c(0L,
                1L,
                seq.int(from = 5L,
                        to = break_max,
                        by = 5L))
    ## make labels for breaks
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = TRUE,
                                  include_na = include_na)
    ## check that all intervals fall within implied breaks
    i_interval <- make_i_interval(low = low,
                                up = up,
                                breaks = breaks,
                                open_first = FALSE,
                                open_last = TRUE)
    is_multiple_intervals <- i_interval == -1L
    i_multiple_intervals <- match(TRUE, is_multiple_intervals, nomatch = 0L)
    if (i_multiple_intervals > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that intersects two or more intervals formed using '%s = %d",
                      "x",
                      labels_x[[i_multiple_intervals ]],
                      "break_max",
                      break_max),
             call. = FALSE)
    ## make labels for these breaks
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = TRUE,
                                  include_na = include_na)
    ## return result
    ans <- labels_new[i_interval][match(x, labels_x)]
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
#' rather than to a specific value, then \code{format_age_births}
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
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_age_births}
#' also contain \code{NA}.
#' 
#' @param x A vector of age group labels. The
#' age of the parent at the time of the birth of
#' the child.
#' @param break_min An integer or \code{NULL}.
#' Defaults to 15.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 50.
#' @param width The width in years of the age
#' groups to be created. A positive integer defaulting to 5.
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
#' \code{\link{date_to_age_year}} calculates
#' ages from dates.
#'
#' @examples
#' format_age_births(x = c(22, 34, 19))
#' 
#' format_age_births(x = c("20-24", "37", NA, "32", "21-24"))
#' 
#' format_age_births(x = c("20-24", "37", "32", "21-24"),
#'                   width = 10,
#'                   break_min = 20)
#'
#' format_age_births(x = c(20, 37, 15),
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
#' format_age_births(x = c("22", "13-14", "55", "10-19"),
#'                   recode_up = TRUE,
#'                   recode_down = TRUE)
#' @export
format_age_births <- function(x,
                              break_min = 15,
                              break_max = 50,
                              width = 5,
                              recode_up = FALSE,
                              recode_down = FALSE) {
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_break_max <- !is.null(break_max)
    ## check arguments
    if (has_break_min)
        break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                                   name = "break_min")
    if (has_break_max)
        break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                               name = "break_max")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if (has_break_min && has_break_max) {
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
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (is_unbounded) {
        if (length(x) == 0L) {
            ans <- factor()
            return(ans)
        }
        if (all(is.na(x))) {
            ans <- factor(x,
                          levels = unique(x),
                          exclude = NULL)
            return(ans)
        }
    }
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## parse labels, and extract information
    parsed <- parse_quantities(x = labels_x,
                               name = "x")
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
    ## check that ages lie within limits implied by 'break_min' and 'break_max'
    if (has_break_min) {
        is_lt_min <- low < break_min
        i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
        if (i_lt_min > 0L) {
            if (recode_up) {
                low[is_lt_min] <- break_min
                up[is_lt_min] <- pmax(up[is_lt_min], low[is_lt_min] + 1L)
            }
            else {
                stop(gettextf("'%s' has interval [\"%s\"] that starts below '%s' [%d] and '%s' is FALSE",
                              "x",
                              labels_x[[i_lt_min]],
                              "break_min",
                              break_min,
                              "recode_up",
                              "FALSE"),
                     call. = FALSE)
            }
        }
    }
    if (has_break_max) {
        is_gt_max <- up > break_max
        i_gt_max <- match(TRUE, is_gt_max, nomatch = 0L)
        if (i_gt_max > 0L) {
            if (recode_down) {
                up[is_gt_max] <- break_max
                low[is_gt_max] <- pmin(low[is_gt_max], up[is_gt_max] - 1L)
            }
            else {
                stop(gettextf("'%s' has interval [\"%s\"] that ends above '%s' [%d] and '%s' is %s",
                              "x",
                              labels_x[[i_gt_max]],
                              "break_max",
                              break_max,
                              "recode_down",
                              "FALSE"),
                     call. = FALSE)
            }
        }
    }
    ## make 'break_min', 'break_max'
    if (!has_break_min) {
        break_min <- break_min_x
        message(gettextf("setting '%s' to %d",
                         "break_min", break_min))
    }
    if (!has_break_max) {
        remainder_max <- (break_max_x - break_min) %% width
        if (remainder_max == 0L)
            break_max <- break_max_x
        else
            break_max <- break_max_x - remainder_max + width
        message(gettextf("setting '%s' to %d",
                         "break_max", break_max))
    }
    ## make 'breaks'
    breaks <- seq.int(from = break_min,
                      to = break_max,
                      by = width)
    ## check that all intervals fall within implied breaks
    i_interval <- make_i_interval(low = low,
                                  up = up,
                                  breaks = breaks,
                                  open_first = FALSE,
                                  open_last = FALSE)
    is_multiple_intervals <- i_interval == -1L
    i_multiple_intervals <- match(TRUE, is_multiple_intervals, nomatch = 0L)
    if (i_multiple_intervals > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that intersects two or more intervals formed using '%s = %d', '%s = %d', and '%s = %d'",
                      "x",
                      labels_x[[i_multiple_intervals ]],                      
                      "break_min",
                      break_min,
                      "break_max",
                      break_max,
                      "width",
                      width),
             call. = FALSE)
    ## make labels for breaks
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = FALSE,
                                  include_na = include_na)
    ## return result
    ans <- labels_new[i_interval][match(x, labels_x)]
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## HAS_TESTS
#' Format age group labels into customized age groups
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
#' second-to-last value for \code{breaks}, and \code{b}
#' is the last value, then the oldest age
#' group is \code{[a, b)} years.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_age_custom}
#' also contain \code{NA}.
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
#' \code{\link{date_to_age_year}} calculates
#' ages from dates.
#'
#' @examples
#' format_age_custom(x = c(22, 11, 85),
#'                   breaks = c(0, 15, 45, 70))
#' format_age_custom(x = c("90+", "19-40", "22", NA),
#'                   breaks = c(0, 15, 60))
#' format_age_custom(x = c("50-59", "19-40", "31"),
#'                   breaks = c(15, 45, 60),
#'                   open_last = FALSE)
#' @export
format_age_custom <- function(x,
                              breaks = NULL,
                              open_last = TRUE) {
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
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## extract lower and upper ages
    parsed <- parse_quantities(x = labels_x,
                               name = "x")
    low <- parsed$low # integer
    up <- parsed$up   # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    i_open_first <- match(TRUE, is_open_first, nomatch = 0L)
    if (i_open_first > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
                      "x", labels_x[[i_open_first]]),
             call. = FALSE)
    }
    ## check that ages lie within limits implied by 'breaks' and 'open_last'
    is_lt_min <- low < breaks[[1L]]
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that starts below the minimum value for '%s' [%d]",
                      "x",
                      labels_x[[i_lt_min]],
                      "breaks",
                      breaks[[1L]]),
             call. = FALSE)
    }
    if (!open_last) {
        is_gt_max <- up > breaks[[n_break]]
        i_gt_max <- match(TRUE, is_gt_max, nomatch = 0L)
        if (i_gt_max > 0L) {
            stop(gettextf("'%s' has interval [\"%s\"] that ends above the maximum value for '%s' [%d]",
                          "x",
                          labels_x[[i_gt_max]],
                          "breaks",
                          breaks[[n_break]]),
                 call. = FALSE)
        }
    }
    ## check that all intervals fall within implied breaks
    i_interval <- make_i_interval(low = low,
                                  up = up,
                                  breaks = breaks,
                                  open_first = FALSE,
                                  open_last = open_last)
    is_multiple_intervals <- i_interval == -1L
    i_multiple_intervals <- match(TRUE, is_multiple_intervals, nomatch = 0L)
    if (i_multiple_intervals > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that intersects two or more intervals formed using '%s'",
                      "x",
                      labels_x[[i_multiple_intervals]],
                      "breaks"),
             call. = FALSE)
    ## make labels for breaks
    include_na <- anyNA(labels_x)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = open_last,
                                  include_na = include_na)
    ## return result
    ans <- labels_new[i_interval][match(x, labels_x)]
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
#' \code{TRUE} (the defaults), then \code{format_age_quarter}
#' creates a factor with levels \code{"0"}, \code{"1"},
#' \dots, \code{"399"}, \code{"400+"}. Even when an age
#' group between \code{break_min} and \code{break_max}
#' is not included in \code{x}, \code{factor_quarter}
#' still creates a level for it.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} quarters
#' old at their most recent birthday. For instance, a person
#' belongs to age group \code{"20"} if that person had
#' their 5th birthday (= 20 quarters) two days ago.
#' 
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{format_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' All age groups in \code{x} must be single-quarter age groups,
#' except those starting at or above \code{break_max},
#' where open age groups are allowed.
#'
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_age_quarter}
#' also contain \code{NA}.
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
#' @examples
#' format_age_quarter(x = c(0, 21, 5))
#' 
#' format_age_quarter(x = c("10", "22", "500+"))
#'
#' ## specify highest age group
#' format_age_quarter(x = c("10", "22", "500+"),
#'                    break_max = 48)
#'
#' ## let lowest age group be determined by the data
#' format_age_quarter(x = c("10", "22", "500+"),
#'                    break_min = NULL,
#'                    break_max = 48)
#'
#' ## make final age group closed
#' format_age_quarter(x = c("10", "22"),
#'                    break_min = NULL,
#'                    break_max = 48,
#'                    open_last = FALSE)
#' @export
format_age_quarter <- function(x, 
                           break_min = 0,
                           break_max = 400,
                           open_last = TRUE) {
    format_age_month_quarter_year(x = x,
                                  break_min = break_min,
                                  break_max = break_max,
                                  open_last = open_last)
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
#' creates a factor with levels \code{"0"}, \code{"1"},
#' \dots, \code{"1199"}, \code{"1200+"}. Even when an age
#' group between \code{break_min} and \code{break_max}
#' is not included in \code{x}, \code{factor_month}
#' still creates a level for it.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} months
#' old at their most recent birthday.
#' For instance, a person belongs to age
#' group \code{"60"} if that person had their
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
#' If \code{x} contains \code{NA}, then the
#' levels of the factor created by \code{format_age_month}
#' also contain \code{NA}.
#'
#' @inheritParams format_age_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 1200.
#'
#' @return A factor with the same length as \code{x}.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{format_age_year}},
#' \code{\link{format_age_multi}},
#' \code{\link{format_age_lifetab}},
#' \code{\link{format_age_births}},
#' \code{\link{format_age_custom}},
#' \code{\link{format_age_quarter}}.
#'
#' @examples
#' format_age_month(x = c(22, 0, 300))
#'
#' format_age_month(x = c("3", NA, "12", "1400+"))
#'
#' ## specify highest age group
#' format_age_month(x = c("3", "12", "1400+"),
#'                  break_max = 24)
#'
#' ## let lowest age group be determined by the data
#' format_age_month(x = c("3", "12", "1400+"),
#'                  break_min = NULL,
#'                  break_max = 24)
#'
#' ## make final age group closed
#' format_age_month(x = c("3", "12"),
#'                  break_min = NULL,
#'                  break_max = 24,
#'                  open_last = FALSE)
#' @export
format_age_month <- function(x,
                             break_min = 0,
                             break_max = 1200,
                             open_last = TRUE) {
    format_age_month_quarter_year(x = x,
                                  break_min = break_min,
                                  break_max = break_max,
                                  open_last = open_last)
}


