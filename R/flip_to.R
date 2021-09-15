
## HAS_TESTS
#' Change the format of one-year period or cohort labels
#'
#' Change one-year period or cohort labels from using
#' the calendar year at the start to using the calendar
#' year at the end, or vice versa.
#'
#' As discussed in \code{\link{date_to_period_year}}
#' and \code{\link{date_to_cohort_year}}, single-year
#' labels are ambiguous. The label \code{"2020"}, for instance,
#' could refer to many different sets of dates, depending on
#' the starting month and on whether the 2020 refers to
#' the calendar year at the start of the period or cohort,
#' or the calendar year at the end.
#'
#' Converting between calendar-year-at-end conventions
#' and calendar-year-at-start conventions is error-prone.
#' \code{flip_to_end} and \code{flip_to_start}
#' try to make the process a little easer. The functions are
#' used as follows
#'
#' \tabular{lll}{
#'   \emph{Current labels} \tab \code{Desired labels} \tab \emph{Function to use} \cr
#'   Use calendar year at start \tab Use calendar year at end \tab \code{flip_to_end} \cr
#'   Use calendar year at end \tab Use calendar year at start \tab \code{flip_to_start} \cr
#' }
#'
#' If \code{month_start} is \code{"Jan"} (so that
#' the labels are the same under both conventions)
#' \code{flip_to_end} and \code{flip_to_start}
#' returns the original labels.
#' 
#' @param x A vector of period or cohort labels
#' that are single-year or open on the left.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Each period/cohort
#' starts on the first day of this month.
#'
#' @return A vector with the same length as \code{x}.
#'
#' @seealso \code{\link{date_to_period_year}},
#' \code{\link{date_to_cohort_year}},
#' \code{\link{format_period_year}},
#' \code{\link{format_cohort_year}}
#'
#' @examples
#' ## currently using calendar year at start of period
#' flip_to_end(x = c("2001", "2005"),
#'             month_start = "Jul")
#'
#' ## currently using calendar year at end of period
#' flip_to_end(x = c("2001", "2005"),
#'             month_start = "Jul")
#'
#' ## periods start in January, so changing labelling
#' ## rule has no effect
#' flip_to_end(x = c("2001", "2005"),
#'             month_start = "Jan")
#' @name flip_to_start
NULL

#' @rdname flip_to_start
#' @export
flip_to_start <- function(x,
                          month_start = "Jan") {
    flip_to_internal(x = x,
                     month_start = month_start,
                     to_end = FALSE)
}

#' @rdname flip_to_start
#' @export
flip_to_end <- function(x,
                        month_start = "Jan") {
    flip_to_internal(x = x,
                     month_start = month_start,
                     to_end = TRUE)
}

