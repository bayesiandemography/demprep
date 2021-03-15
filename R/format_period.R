## ## HAS_TESTS
## #' Put period labels into the format required
## #' for multi-year periods
## #'
## #' Given a vector of period labels, create a factor
## #' that contains levels for all periods between the
## #' earliest and latest periods in \code{x}.
## #' For instance, if the earliest period in \code{x}
## #' is \code{"1990-1995"}, and the latest is \code{"2005-2010"},
## #' then \code{format_period_multi} creates a factor
## #' with levels \code{"1990-1995"}, \code{"1990-1995"},
## #' \code{"1990-1995"},and the latest is \code{"2005-2010"},
## #' \code{break_max} is \code{100}, \code{width} is \code{5},
## #' and \code{open_last} is \code{TRUE} (the defaults),
## #' then \code{format_age_multi} creates
## #' a factor with levels \code{"0-4"}, \code{"5-9"},
## #' \dots, \code{"95-99"}, \code{"100+"}. Even when an age
## #' group between \code{break_min} and \code{break_max}
## #' is not included in the data \code{x}, \code{factor_age_year}
## #' still creates a level for it.
## #' 
## #' If \code{break_min} or \code{break_max} is set to \code{NULL},
## #' rather than to a specific value, then \code{format_age_year}
## #' finds the narrowest range that accommodates the data.
## #'
## #' All age groups in \code{x} must fall within the intervals
## #' determined by \code{break_min} and \code{width},
## #' except for age groups above \code{break_max}.
## #'
## #' @inheritParams format_age_year
## #' @param width The width in years of the age intervals.
## #' A positive integer. Defaults to 5.
## #'
## #' @return A factor with the same length as
## #' \code{x}.
## #'
## #' @seealso Other functions for reformating
## #' age group labels are 
## #' \code{\link{format_age_year}},
## #' \code{\link{format_age_lifetab}},
## #' \code{\link{format_age_births}},
## #' \code{\link{format_age_custom}},
## #' \code{\link{format_age_quarter}},
## #' and \code{\link{format_age_month}}.
## #'
## #' \code{\link{date_to_age_multi}} creates
## #' multi-year age groups from dates.
## #'
## #' \code{\link{make_labels_age}} describes the rules
## #' for constructing labels for age groups.
## #' @examples
## #' format_age_multi(x = c("10-14", "3", "100+", "77-79"))
## #' 
## #' format_age_multi(x = c("10-14", "3", "100+", "77-79"),
## #'                  width = 10)
## #'
## #' ## age groups wider than the 'width' argument
## #' ## are allowed, provided they are above 'break_max'
## #' format_age_multi(x = c("15", "30-39", "45-59"),
## #'                  width = 10,
## #'                  break_max = 40)
## #'
## #' ## allow 'break_min' and 'break_max' to be
## #' ## determined by the data
## #' format_age_multi(x = c("22", "7", "30"),
## #'                  break_min = NULL,
## #'                  break_max = NULL)
## #'
## #' ## allow 'break_max' to be determined
## #' ## by the data, which includes an
## #' ## open age group
## #' format_age_multi(x = c("17", "10+"),
## #'                  break_max = NULL)
## #'
## #' ## oldest age group is closed
## #' format_age_multi(x = c("10", "3", "77"),
## #'                  open_last = FALSE)
## #' @export 
## format_age_multi <- function(x,
##                              width = 5, 
##                              break_min = 0,
##                              break_max = 100,
##                              open_last = TRUE) {
##     ## regexp patterns
##     p_single <- "^[0-9]+$"
##     p_low_up <- "^([0-9]+)-([0-9]+)$"
##     p_open <- "[0-9]+\\+$"
##     ## check arguments
##     width <- demcheck::err_tdy_positive_integer_scalar(x = width,
##                                                        name = "width",
##                                                        null_ok = TRUE)
##     break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
##                                                                name = "break_min",
##                                                                null_ok = TRUE)
##     break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
##                                                            name = "break_max",
##                                                            null_ok = TRUE)
##     if (!is.null(break_min) && !is.null(break_max)) {
##         demcheck::err_lt_scalar(x1 = break_min,
##                                 x2 = break_max,
##                                 name1 = "break_min",
##                                 name2 = "break_max")
##         demcheck::err_difference_divisible(x1 = break_max,
##                                            x2 = break_min,
##                                            y = width,
##                                            name1 = "break_max",
##                                            name2 = "break_min",
##                                            name_y = "width")
##     }
##     demcheck::err_is_logical_flag(x = open_last,
##                                   name = "open_last")
##     ## deal with "empty" case where 'x' has no non-NA values
##     ## and 'break_min' or 'break_max' is missing
##     ## (so cannot construct levels)
##     n <- length(x)
##     all_empty <- (n == 0L) || all(is.na(x))
##     is_unbounded <- is.null(break_min) || is.null(break_max)
##     if (all_empty && is_unbounded) {
##         ans <- rep(NA_character_, times = n)
##         ans <- factor(ans,
##                       levels = NA_character_,
##                       exclude = NULL)
##         return(ans)
##     }
##     ## put unique values in 'labels_old' vector
##     labels_old <- unique(x)
##     ## classify labels_old, raising error for any invalid ones
##     is_na <- is.na(labels_old)
##     is_single <- grepl(p_single, labels_old)
##     is_low_up <- grepl(p_low_up, labels_old)
##     is_open <- grepl(p_open, labels_old)
##     is_valid <- is_na | is_single | is_low_up | is_open
##     i_invalid <- match(FALSE, is_valid, nomatch = 0L)
##     if (i_invalid > 0L)
##         stop(gettextf("\"%s\" is not a valid label for an age group",
##                       labels_old[[i_invalid]]),
##              call. = FALSE)
##     ## extract lower and upper ages
##     age_low <- rep(NA_integer_, times = length(labels_old))
##     age_up <- age_low
##     age_low[is_single] <- as.integer(labels_old[is_single])
##     age_up[is_single] <- age_low[is_single] + 1L
##     age_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels_old[is_low_up]))
##     age_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels_old[is_low_up])) + 1L
##     age_low[is_open] <- as.integer(sub("\\+", "", labels_old[is_open]))
##     demcheck::err_age_diff_gt_one(age_low = age_low,
##                                   age_up = age_up,
##                                   is_low_up = is_low_up,
##                                   labels = labels_old)
##     ## if 'break_min' is supplied, make sure no
##     ## intervals less than 'break_min'
##     if (!is.null(break_min)) {
##         demcheck::chk_age_label_ge_break_min(labels = labels_old,
##                                        age_low = age_low,
##                                        break_min = break_min)
##     }
##     ## if 'open_last' is FALSE, check that there
##     ## are no open age groups
##     if (!open_last) {
##         demcheck::err_no_open_age(labels_old)
##     }
##     ## if 'open_last' is FALSE, and 'break_max' is supplied,
##     ## make sure that all intervals less than 'break_max'
##     if (!open_last && !is.null(break_max)) {
##         demcheck::chk_age_label_le_break_max(labels = labels_old,
##                                        age_up = age_up,
##                                        break_max = break_max)
##     }
##     ## make breaks
##     breaks <- make_breaks_label_to_integer_year(age_low = age_low,
##                                                 age_up = age_up,
##                                                 labels = labels,
##                                                 width = width,
##                                                 is_open = is_open,
##                                                 break_min = break_min,
##                                                 break_max = break_max,
##                                                 open_last = open_last)
##     ## make labels for these breaks
##     include_na <- any(is_na)
##     labels_new <- make_labels_age(breaks = breaks,
##                                   open_last = open_last,
##                                   include_na = include_na)
##     ## assign new labels to x
##     i_label_old <- match(x, labels_old)
##     age <- age_low[i_label_old]
##     i_intervals_new <- findInterval(x = age,
##                                     vec = breaks)
##     ans <- labels_new[i_intervals_new]
##     ## return result
##     ans <- factor(x = ans,
##                   levels = labels_new,
##                   exclude = NULL)
##     ans
## }
