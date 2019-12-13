
## HAS_TESTS
#' Make labels for age groups measured in years
#'
#' Make labels for age groups with lengths measured in
#' whole years. The labels follow standard
#' demographic conventions.
#' 
#' Age groups are defined via the \code{breaks} argument.
#' The elements of \code{breaks} must be non-negative integers,
#' and must be strictly increasing.  
#' Labels for single-year age groups, eg from birth to exact age 1, from
#' exact age 1 to exact age 2, and so on, take the form
#' \code{"0", "1", "2", ...}. Labels for multi-year age groups,
#' eg from birth to exact age 5, from exact age 5 to
#' exact age 10, and so on, take the form \code{"0-4", "5-9", ...}.
#'
#' When \code{open_last} is \code{TRUE}, an 'open' age
#' group with no upper limit is appended to the end of the labels.
#' By default, \code{open_last} is \code{TRUE}.
#' 
#' When \code{include_na} is \code{TRUE}, an \code{NA}
#' is added to the end of the labels. This can be useful
#' when dealing with dates that include \code{NA}s.
#'
#' \code{breaks} can only have length 0 if \code{open_last}
#' is \code{FALSE}. \code{breaks} can
#' have only have length 1 if \code{open_last}
#'  is TRUE.
#'
#' @param breaks A integer vector, or a vector that can
#' be coerced to integer via function \code{\link[base]{as.integer}}.
#' @param open_last Whether to append an open-ended
#' age group to the end of the labels.
#' Defaults to \code{TRUE}.
#' @param include_na  Whether to append an \code{NA} to
#' the end of the labels. Defaults to \code{FALSE}.
#'
#' @return A character vector. 
#'
#' @seealso To make labels for periods measured in years, use
#' \code{\link{make_labels_period}}. There is
#' no \code{make_labels_cohort} function. To construct
#' labels for cohorts, just use \code{make_labels_period}.
#' To make labels for age groups with widths of one quarter or one month,
#' use functions \code{\link{make_labels_age_group_quarter}} or
#' \code{\link{make_labels_age_group_month}}.
#'
#' @examples
#' ## single-year
#' make_labels_age_group(breaks = 0:100)
#'
#' ## 5-year
#' make_labels_age_group(breaks = seq(0, 80, 5))
#'
#' ## mixed
#' make_labels_age_group(breaks = c(0, 1, 4, seq(5, 80, 5)))
#'
#' ## no open age group
#' make_labels_age_group(breaks = seq(15, 65, 5))
#' 
#' ## allow for missing
#' make_labels_age_group(breaks = seq(15, 65, 5),
#'                       include_na = TRUE)
#' @export
make_labels_age_group <- function(breaks,
                                  open_last = TRUE,
                                  include_na = FALSE) {
    breaks <- demcheck::err_tdy_breaks_integer_age(breaks = breaks,
                                                   open_last = open_last)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    make_labels_grouped_int_enumerations(breaks - breaks,
                                         open_first = FALSE,
                                         open_last = open_last,
                                         include_na = include_na)
}

## HAS_TESTS
#' Make labels for age groups measured in quarters
#'
#' Make labels for age groups with lengths measured in
#' quarters (ie three months).
#' 
#' A person's age group, measured in quarters, is the
#' number of quarters that have elapsed since that person was
#' born. For instance, the age in quarters of a person
#' born on 10 February 2010 is as follows:
#' \tabular{ll}{
#'   \strong{Date}  \tab \strong{Age} \cr
#'   10 February 2010 - 9 May 2010 \tab 0q \cr
#'   10 May 2010 - 9 August 2010 \tab 1q \cr
#'   10 August 2010 - 9 November 2010 \tab 2q \cr
#'   10 November 2010 - 9 February 2011 \tab 3q \cr
#'   10 February 2011 - 9 May 2011 \tab 4q \cr
#'   etc \tab etc
#' }
#'
#' The lowest and highest age groups are defined by
#' the \code{break_min} and \code{break_max} arguments,
#' along with the \code{open_last} argument. The youngest age group
#' consists of everyone aged at least \code{break_min}
#' quarters. When \code{open_last} is \code{TRUE} (the default)
#' the oldest age group consists of everyone aged at least
#' \code{break_max} quarters, with no upper limit. When
#' \code{open_last} is \code{FALSE}, the oldest age group
#' consists of everyone aged at least \code{break_max - 1} quarters
#' but less than \code{break_max} quarters.
#'
#' When \code{include_na} is \code{TRUE}, an \code{NA}
#' is added to the end of the labels. This can be useful
#' when dealing with dates that include \code{NA}s.
#'
#' @inheritParams make_labels_age_group
#' @param break_min An integer. The lower limit (in quarters) of
#' the youngest age group. Defaults to \code{0}.
#' @param break_max An integer. If \code{open_last} is \code{TRUE},
#' the lower limit (in quarters) of the oldest age group;
#' otherwise the upper limit of the oldest age group.
#' Defaults to \code{400}.
#'
#' @return A character vector. 
#'
#' @seealso To make labels for periods measured in quarters, use
#' \code{\link{make_labels_period_quarter}}. There is
#' no \code{make_labels_cohort_quarter} function. To construct
#' labels for quarter cohorts, just use \code{make_labels_period_quarter}.
#' To make labels for age groups measured in years,
#' use function \code{\link{make_labels_age_group}}, and
#' to make labels for age groups measured in months,
#' use function \code{\link{make_labels_age_group_month}}.
#'
#' @examples
#' make_labels_age_group_quarter(break_max = 10)
#' make_labels_age_group_quarter(break_max = 10,
#'                               open_last = FALSE)
#' make_labels_age_group_quarter(break_min = 5,
#'                               break_max = 10,
#'                               open_last = FALSE)
#' make_labels_age_group_quarter(break_max = 10,
#'                               include_na = TRUE)
#' @export
make_labels_age_group_quarter <- function(break_min = 0,
                                          break_max = 400,
                                          open_last = TRUE,
                                          include_na = FALSE) {
    l <- demcheck::err_tdy_break_min_max_integer(break_min = break_min,
                                                 break_max = break_max,
                                                 null_ok = FALSE,
                                                 equal_ok = open_last)
    break_min <- l$break_min
    break_max <- l$break_max
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    make_labels_duration_quarters_months(break_min = break_min,
                                         break_max = break_max,
                                         open_first = FALSE,
                                         open_last = open_last,
                                         include_na = include_na,
                                         unit = "quarter")
}

## HAS_TESTS
#' Make labels for age groups measured in months
#'
#' Make labels for age groups with lengths measured in
#' months.
#' 
#' A person's age group, measured in months, is the
#' number of months that have elapsed since that person was
#' born. For instance, the age in months of a person
#' born on 10 February 2010 is as follows:
#' \tabular{ll}{
#'   \strong{Date}  \tab \strong{Age} \cr
#'   10 February 2010 - 9 March 2010 \tab 0m \cr
#'   10 March 2010 - 9 April 2010 \tab 1m \cr
#'   10 April 2010 - 9 May 2010 \tab 2m \cr
#'   10 May 2010 - 9 June 2010 \tab 3m \cr
#'   10 June 2010 - 9 July 2010 \tab 4m \cr
#'   etc \tab etc
#' }
#'
#' The lowest and highest age groups are defined by
#' the \code{break_min} and \code{break_max} arguments,
#' along with the \code{open_last} argument. The youngest age group
#' consists of everyone aged at least \code{break_min}
#' months. When \code{open_last} is \code{TRUE} (the default)
#' the oldest age group consists of everyone aged at least
#' \code{break_max} months, with no upper limit. When
#' \code{open_last} is \code{FALSE}, the oldest age group
#' consists of everyone aged at least \code{break_max - 1} months
#' but less than \code{break_max} months.
#'
#' When \code{include_na} is \code{TRUE}, an \code{NA}
#' is added to the end of the labels. This can be useful
#' when dealing with dates that include \code{NA}s.
#'
#' @inheritParams make_labels_age_group
#' @param break_min An integer. The lower limit (in months) of
#' the youngest age group. Defaults to \code{0}.
#' @param break_max An integer. If \code{open_last} is \code{TRUE},
#' the lower limit (in months) of the oldest age group;
#' otherwise the upper limit of the oldest age group.
#' Defaults to \code{1200}.
#'
#' @return A character vector. 
#'
#' @seealso To make labels for periods measured in months, use
#' \code{\link{make_labels_period_month}}. There is
#' no \code{make_labels_cohort_month} function. To construct
#' labels for month cohorts, just use \code{make_labels_period_month}.
#' To make labels for age groups measured in years,
#' use function \code{\link{make_labels_age_group}}, and
#' to make labels for age groups measured in quarters,
#' use function \code{\link{make_labels_age_group_quarter}}.
#'
#' @examples
#' make_labels_age_group_month(break_max = 10)
#' make_labels_age_group_month(break_max = 10,
#'                             open_last = FALSE)
#' make_labels_age_group_month(break_min = 5,
#'                             break_max = 10,
#'                             open_last = FALSE)
#' make_labels_age_group_month(break_max = 10,
#'                             include_na = TRUE)
#' @export
make_labels_age_group_month <- function(break_min = 0,
                                        break_max = 1200,
                                        open_last = TRUE,
                                        include_na = FALSE) {
    l <- demcheck::err_tdy_break_min_max_integer(break_min = break_min,
                                                 break_max = break_max,
                                                 null_ok = FALSE,
                                                 equal_ok = open_last)
    break_min <- l$break_min
    break_max <- l$break_max
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    make_labels_duration_quarters_months(break_min = break_min,
                                         break_max = break_max,
                                         open_first = FALSE,
                                         open_last = open_last,
                                         include_na = include_na,
                                         unit = "month")
}
