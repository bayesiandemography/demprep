
## HAS_TESTS
#' Change format of one-year period or cohort labels
#'
#' Change one-year period or cohort labels from using
#' calendar year at the start of the period/cohort
#' to calendar year at the end, or vice versa.
#'
#' As discussed in \code{\link{date_to_period_year}}
#' and \code{\link{date_to_cohort_year}}, single-year
#' labels are ambiguous. The label \code{"2020"}, for instance,
#' could refer to any period from
#' "1 February 2019 - 31 January 2020" to
#' "1 December 2020 - 31 November 2021", depending on
#' the starting month and on whether the label
#' uses the calendar year at the start of the period/cohort
#' or the calendar year at the end.
#'
#' If \code{current_uses_start} is \code{TRUE},
#' then \code{flip_year_label} produces new
#' labels that use calendar year at the end
#' of the period/cohort. If \code{current_uses_start}
#' is \code{FALSE}, then \code{flip_year_label}
#' produces new labels that use calendar year at the
#' start of the period/cohort. 
#'
#' @param x A vector of period or cohort labels
#' that are single-year or open on the left.
#' @param current_uses_start Logical. Whether the
#' the current labels use calendar year at
#' the start of the period/cohort.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Each period/cohort
#' starts on the first day of this month.
#'
#' @return A factor with the same length as \code{x}.
#'
#' @seealso \code{\link{date_to_period_year}},
#' \code{\link{date_to_cohort_year}},
#' \code{\link{format_period_year}},
#' \code{\link{format_cohort_year}}
#'
#' @examples
#' ## currently using calendar year at start of period
#' flip_year_label(x = c("2001", "2005"),
#'                 current_uses_start = TRUE,
#'                 month_start = "Jul")
#'
#' ## currently using calendar year at end of period
#' flip_year_label(x = c("2001", "2005"),
#'                 current_uses_start = FALSE,
#'                 month_start = "Jul")
#'
#' ## periods start in January, so changing labelling
#' ## rule has no effect
#' flip_year_label(x = c("2001", "2005"),
#'                 current_uses_start = TRUE,
#'                 month_start = "Jan")
#' @export
flip_year_label <- function(x,
                            current_uses_start = TRUE,
                            month_start = "Jan") {
    demcheck::err_is_logical_flag(x = current_uses_start,
                                  name = "current_uses_start")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    do_nothing <- identical(month_start, "Jan") || identical(length(x), 0L)
    if (do_nothing) {
        ans <- if (is.factor(x)) x else factor(x, exclude = NULL)
        return(ans)
    }
    labels_x <- if (is.factor(x)) levels(x) else unique(x)
    parsed <- parse_integers(x = labels_x,
                             name = "x")
    low <- parsed$low
    up <- parsed$up
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", labels_x[[i_open_last]]),
             call. = FALSE)
    if (current_uses_start) {
        labels_ans <- up
        labels_ans[is_open_first] <- paste0("<", up[is_open_first] + 1L)
    }
    else {
        labels_ans <- low - 1L
        labels_ans[is_open_first] <- paste0("<", up[is_open_first] - 1L)
    }
    ans <- factor(x,
                  levels = labels_x,
                  labels = labels_ans,
                  exclude = NULL)
    ans
}
