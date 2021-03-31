
## HAS_TESTS
#' Make labels for cohorts measured in years
#'
#' Make labels for cohorts with lengths measured in whole years.
#' The function would not normally be
#' called directly by end users.
#'  
#' Cohorts are defined by the dates specified in \code{breaks}.
#' A cohort ordinarily starts at one date in \code{breaks} and ends
#' the day before the next date. For instance, if \code{breaks} is
#' \code{c("2000-01-01", "2001-01-01", "2002-01-01")}, then
#' one cohort begins on 2000-01-01 and ends on 2000-12-31,
#' and the other starts on 2001-01-01 and ends on 2001-12-31.
#' However, when \code{open_first} is \code{TRUE}, an 'open' cohort
#' extending indefinitely into the past is appended to the start
#' of the series. This cohort corresponds to the open age group
#' in age group labels. By default, \code{open_first} is \code{FALSE}.
#'
#' If all cohorts have widths of one year,
#' then the labels consist of a single years, eg
#' \code{"2020", "2021", "2020"}. Otherwise, the
#' labels consist of start years and end years
#' separated by dashes, eg \code{"2020-2025", "2025-2030", "2030-2035"}.
#'
#' Unfortunately, the practice of using a single year to label
#' one-year cohorts can be ambiguous.
#' If a cohort starts on 2020-01-01 and ends on 2020-12-31,
#' then it is clear that the single-year
#' label should be \code{"2020"}. However, if a cohort
#' starts on 2020-07-01 and ends on 2021-06-30,
#' some people use the calendar year at the
#' \emph{start}, so that the label is
#' \code{"2020"}, and others use the calendar
#' year at the \emph{end}, so that the label
#' is \code{"2021"}. (In the latter case,
#' cohorts are often called "years to", eg "years to June".)
#'
#' \code{make_labels_cohort} by default uses the start year
#' to make single-year labels. To use the end year,
#' set \code{label_year_start} to \code{FALSE}.
#' If \code{open_first} is \code{TRUE}, then
#' \code{label_year_start} must be \code{TRUE}.
#'
#' When \code{include_na} is \code{TRUE}, an \code{NA}
#' is added to the end of the labels. This can be useful
#' when dealing with dates that include \code{NA}s.
#'
#' If \code{breaks} has length 0 then \code{open_first}
#' must be \code{FALSE}, and if \code{breaks} has
#' length 1, then \code{open_first} must be \code{TRUE}.
#'
#' @param breaks A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param open_first Whether to append an open-ended
#' cohort to the start of the labels.
#' Defaults to \code{FALSE}.
#' @param label_year_start Whether to label a cohort
#' by the calendar year at the beginning of the cohort.
#' Not needed for multi-year cohorts, or for single-year
#' cohorts that start on 1 January.
#' Defaults to \code{TRUE}.
#' @param include_na  Whether to append an \code{NA} to
#' the end of the labels. Defaults to \code{FALSE}.
#'
#' @return A character vector. 
#'
#' @seealso To make labels for age groups measured in years,
#' use \code{\link{make_labels_age}}, and to make
#' labels for periods measured in years, use 
#' \code{make_labels_period}.
#' To make labels for cohorts with widths of one quarter or one month,
#' use functions \code{\link{make_labels_cohort_quarter}} or
#' \code{\link{make_labels_cohort_month}}.
#'
#' @examples
#' ## 5-year cohorts
#' make_labels_cohort(breaks = c("2005-01-01",
#'                               "2010-01-01",
#'                               "2015-01-01",
#'                               "2020-01-01"))
#' 
#' ## one-year cohorts, using default setting where
#' ## labels use calender year at start 
#' make_labels_cohort(breaks = c("2005-07-01",
#'                               "2006-07-01",
#'                               "2007-07-01"))
#' 
#' ## this time with label using
#' ## calendar year at end
#' make_labels_cohort(breaks = c("2005-07-01",
#'                               "2006-07-01",
#'                               "2007-07-01"),
#'                    label_year_start = FALSE)
#'
#' ## add an open cohort at the beginning
#' make_labels_cohort(breaks = c("2005-01-01",
#'                               "2010-01-01",
#'                               "2015-01-01",
#'                               "2020-01-01"),
#'                    open_first = TRUE)
#'
#' ## add an 'NA' label
#' make_labels_cohort(breaks = c("2005-07-01",
#'                               "2006-07-01",
#'                               "2007-07-01"),
#'                    include_na = TRUE)
#' @keywords internal
#' @export
make_labels_cohort <- function(breaks,
                               open_first = FALSE,
                               label_year_start = TRUE,
                               include_na = FALSE) {
    breaks <- demcheck::err_tdy_breaks_date_cohort(breaks = breaks,
                                                   open_first = open_first) 
    demcheck::err_first_day_unit_vector(x = breaks,
                                        name = "breaks",
                                        unit = "year")
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    if (open_first && isFALSE(label_year_start))
        stop(gettextf("'%s' is %s but '%s' is %s",
                      "open_first", "TRUE", "label_year_start", "FALSE"))
    breaks_year <- as.integer(format(breaks, "%Y"))
    n <- length(breaks)
    all_single <- (n > 1L) && all(diff(breaks_year) == 1L)
    if (all_single) {
        demcheck::err_is_logical_flag(x = label_year_start,
                                      name = "label_year_start")
        break_min <- breaks[[1L]]
        is_1_jan <- identical(format(break_min, "%m-%d"), "01-01")
        use_lower_for_single <- label_year_start || is_1_jan
        if (use_lower_for_single) {
            int_min <- breaks_year[[1L]]
            int_max <- breaks_year[[n - 1L]]
        }
        else {
            int_min <- breaks_year[[2L]]
            int_max <- breaks_year[[n]]
        }
        if (open_first) {
            breaks_int <- seq.int(from = int_min,
                                  to = int_max + 1L)
            make_labels_grouped_int_enumerations(breaks = breaks_int,
                                                 open_first = TRUE,
                                                 open_last = FALSE,
                                                 include_na = include_na)
        }
        else            
            make_labels_integers(int_min = int_min,
                                 int_max = int_max,
                                 include_na = include_na)
    }
    else
        make_labels_grouped_int_endpoints(breaks = breaks_year,
                                          open_first = open_first,
                                          open_last = FALSE,
                                          include_na = include_na)
}

## HAS_TESTS
#' Make labels for cohorts with lengths of one quarter
#'
#' Make labels for cohorts that are all one quarter long.
#' The function would not normally be
#' called directly by end users.
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
#' \code{break_min} and \code{break_max},
#' together with \code{open_first},
#' define the lower and upper limits of the cohorts.
#' \code{break_min} and \code{break_max} must both
#' be the start dates of quarters.
#' When \code{open_first} is \code{TRUE}, an 'open' cohort
#' extending indefinitely into the past is appended to the start
#' of the labels. This cohort corresponds to the open age group
#' in age group labels.
#' By default, \code{open_first} is \code{FALSE}.
#'
#' When \code{include_na} is \code{TRUE}, an \code{NA}
#' is added to the end of the labels. This can be useful
#' when dealing with data that include \code{NA}s.
#'
#' @inheritParams make_labels_cohort
#' @param break_min A value of class \code{\link[base]{Date}},
#' or a value that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param break_max A value of class \code{\link[base]{Date}},
#' or a value that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' @return A character vector. 
#'
#' @seealso To make labels for quarter age groups, use
#' \code{\link{make_labels_age_quarter}},
#' and to make labels for quarter periods, use
#' \code{\link{make_labels_period_quarter}}.
#' To make labels for cohorts with widths measured in years,
#' use function \code{\link{make_labels_cohort}}, and to make
#' labels for month cohorts, use \code{\link{make_labels_cohort_month}}.
#'
#' @examples
#' make_labels_cohort_quarter(break_min = "2005-01-01",
#'                            break_max = "2006-07-01")
#' 
#' ## add an open cohort at the beginning
#' make_labels_cohort_quarter(break_min = "2005-04-01",
#'                            break_max = "2006-04-01",
#'                            open_first = TRUE)
#'
#' ## add an 'NA' label
#' make_labels_cohort_quarter(break_min = "2005-04-01",
#'                            break_max = "2006-04-01",
#'                            include_na = TRUE)
#' @keywords internal
#' @export
make_labels_cohort_quarter <- function(break_min,
                                       break_max,
                                       open_first = FALSE,
                                       include_na = FALSE) {
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    l <- demcheck::err_tdy_break_min_max_date(break_min = break_min,
                                              break_max = break_max,
                                              unit = "quarter",
                                              null_ok = FALSE,
                                              equal_ok = open_first)
    break_min <- l$break_min
    break_max <- l$break_max
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")    
    make_labels_calendar_quarters_months(break_min = break_min,
                                         break_max = break_max,
                                         open_first = open_first,
                                         open_last = FALSE,
                                         include_na = include_na,
                                         unit = "quarter")
}

## HAS_TESTS
#' Make labels for cohorts lengths of one month
#'
#' Make labels for cohorts that are all one month long.
#' The function would not normally be
#' called directly by end users.
#'
#' \code{break_min} and \code{break_max},
#' together with \code{open_first},
#' define the lower and upper limits of the cohorts.
#' \code{break_min} and \code{break_max} must both
#' be the start dates of months.
#' When \code{open_first} is \code{TRUE}, an 'open' cohort
#' extending indefinitely into the past is appended to the start
#' of the labels. This cohort corresponds to the open age group
#' in age group labels.
#' By default, \code{open_first} is \code{FALSE}.
#'
#' When \code{include_na} is \code{TRUE}, an \code{NA}
#' is added to the end of the labels. This can be useful
#' when dealing with data that include \code{NA}s.
#'
#' @inheritParams make_labels_cohort
#' @param break_min A value of class \code{\link[base]{Date}},
#' or a value that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param break_max A value of class \code{\link[base]{Date}},
#' or a value that can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' @return A character vector. 
#'
#' @seealso To make labels for month age groups, use
#' \code{\link{make_labels_age_month}}, and to
#' make labels for month periods, use
#' \code{make_labels_period_month}.
#' To make labels for cohorts with widths measured in years,
#' use function \code{\link{make_labels_cohort}}, and to make
#' labels for quarter cohorts, use
#' \code{\link{make_labels_cohort_quarter}}.
#'
#' @examples
#' make_labels_cohort_month(break_min = "2005-01-01",
#'                          break_max = "2005-08-01")
#' 
#'
#' ## add an open cohort at the beginning
#' make_labels_cohort_month(break_min = "2005-03-01",
#'                          break_max = "2006-04-01",
#'                          open_first = TRUE)
#'
#' ## add an 'NA' label
#' make_labels_cohort_month(break_min = "2005-03-01",
#'                          break_max = "2006-04-01",
#'                          include_na = TRUE)
#' @keywords internal
#' @export
make_labels_cohort_month <- function(break_min,
                                     break_max,
                                     open_first = FALSE,
                                     include_na = FALSE) {
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    l <- demcheck::err_tdy_break_min_max_date(break_min = break_min,
                                              break_max = break_max,
                                              unit = "month",
                                              null_ok = FALSE,
                                              equal_ok = open_first)
    break_min <- l$break_min
    break_max <- l$break_max
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")    
    make_labels_calendar_quarters_months(break_min = break_min,
                                         break_max = break_max,
                                         open_first = open_first,
                                         open_last = FALSE,
                                         include_na = include_na,
                                         unit = "month")
}
