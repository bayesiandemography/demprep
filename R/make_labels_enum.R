
## NO_TESTS
#' Make labels for general numeric quantities
#'
#' Make labels for numeric quantities, broadly defined.
#' The quantities may be combined to form intervals,
#' including intervals that have no lower limit or
#' no upper limit. Negative values are allowed,
#' but fractions are not.
#'
#' The labels for intervals follow the same conventions
#' as age groups. Single-value intervals are represented
#' by single integers. Multiple-value intervals are
#' represented by intervals that start with the
#' lower value and end with the upper value minus one.
#' See below for examples.
#'
#' When \code{open_first} is \code{TRUE}, an 'open' interval
#' with no lower limit is appended to the start of the
#' labels. When \code{open_last} is \code{TRUE}, an open interval
#' with no upper limit is appended to the end of the
#' labels.
#' 
#' When \code{include_na} is \code{TRUE}, an \code{NA}
#' is added to the end of the labels. This can be useful
#' when dealing with data that include \code{NA}s.
#'
#' \code{breaks} can only have length 0 if
#' \code{open_first} and \code{open_last} are both
#' \code{FALSE}. \code{breaks} can have only have
#' length 1 if one or both of \code{open_first} and
#' \code{open_last} is \code{TRUE}.
#'
#' @param breaks A integer vector, or a vector that can
#' be coerced to integer via function \code{\link[base]{as.integer}}.
#' @param open_first Whether to append an open-ended
#' interval to the start of the labels.
#' Defaults to \code{FALSE}.
#' @param open_last Whether to append an open-ended
#' interval to the end of the labels.
#' Defaults to \code{FALSE}.
#' @param include_na  Whether to append an \code{NA} to
#' the end of the labels. Defaults to \code{FALSE}.
#'
#' @return A character vector. 
#'
#' @seealso To make labels for age groups, periods,
#' or cohorts, it is generally better to use
#' use specialist functions such as \code{\link{make_labels_age_group}}
#' or \code{\link{make_labels_period}}.
#'
#' @examples
#' make_labels_enum(breaks = c(10, 100, 1000))
#' make_labels_enum(breaks = c(10, 100, 1000),
#'                 open_last = TRUE)
#' make_labels_enum(breaks = seq(50, 100, 10))
#' make_labels_enum(breaks = 0:10,
#'                 open_last = TRUE)
#' make_labels_enum(breaks = c(-1000, 0, 1000),
#'                 open_first = TRUE,
#'                 open_last = TRUE,
#'                 include_na = TRUE)
#' @export
make_labels_enum <- function(breaks,
                             open_first = FALSE,
                             open_last = FALSE,
                             include_na = FALSE) {
    breaks <- demcheck::err_tdy_breaks_integer_enum(breaks = breaks,
                                                    open_first = open_first,
                                                    open_last = open_last)
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    make_labels_grouped_int_enumerations(breaks = breaks,
                                         open_first = open_first,
                                         open_last = open_last,
                                         include_na = include_na)
}
