
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
#' \code{flip_to_end} and \code{flip_to_start}
#' are used convert between the calendar-year-at-start
#' and calendar-year-at-end methods of labelling
#' single-year periods and cohorts:
#'
#' \tabular{lll}{
#'   Current labels \tab Desired labels \tab Function \cr
#'   Use calendar year at start \tab Use calendar year at end \tab \code{flip_to_end} \cr
#'   Use calendar year at end \tab Use calendar year at start \tab \code{flip_to_start} \cr
#' }
#'
#' If \code{month_start} is \code{"Jan"} (so that
#' the labels are the same regardless of what convention
#' is used) \code{flip_to_end} and \code{flip_to_start}
#' do not change the labels in \code{x}.
#' 
#' @param x A vector of period or cohort labels
#' that are single-year or open on the left.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Each period/cohort
#' starts on the first day of this month.
#'
#' @return An If \code{x} is a factor, then a factor.
#' Otherwise a character vector. 
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
#' @name flip_to
NULL

#' @rdname flip_to
#' @export
flip_to_start <- function(x,
                          month_start = "Jan") {
    flip_to_internal(x = x,
                     month_start = month_start,
                     to_end = FALSE)
}

#' @rdname flip_to
#' @export
flip_to_end <- function(x,
                        month_start = "Jan") {
    flip_to_internal(x = x,
                     month_start = month_start,
                     to_end = TRUE)
}

