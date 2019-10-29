
## HAS_TESTS
#' Convert dates to one-year periods
#'
#' Allocate dates to one-year periods. The one-year periods are,
#' by default, calendar years.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' Periods start on the first day of \code{month_start},
#' and end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' If a period starts on 1 January, then the first day and last day
#' of the period belong to the same calendar year.
#' But if a period starts on any other day, then
#' the first day belongs to one calendar
#' year and the last day belongs to the next calendar year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, then the first
#' day belongs to the year 2000, and the last
#' day belongs to the year 2001. Some authors
#' use the first year to label such periods,
#' and others use the last year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, some authors
#' label the period \code{"2000"}, and
#' others label it \code{"2001"}. Function
#' \code{date_to_period_year} by default uses
#' the start year. To use the end year, set
#' \code{label_year_start} to \code{FALSE}.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate periods,
#' including periods that not appear in the data.
#'
#' @param date Dates of events or measurements.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The period starts on
#' the first day of this month.
#' @param label_year_start Logical. Whether to label a period
#' by the calendar year at the beginning of the period
#' or the calendar year at the end. Not needed for periods
#' that start on 1 January.
#' @param as_factor Whether the return value is a factor.
#' Defaults to \code{TRUE}.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_custom}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_period_month}}.
#' Other functions for working with one-year intervals are
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods.
#'
#' @examples
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## July to June
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul")
#'
#' ## July to June, using the calendar year at
#' ## the end for the label
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#'
#' ## return non-factor
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     as_factor = FALSE)
#' @export
date_to_period_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE,
                                as_factor = TRUE) {
    date_to_period_or_cohort_year(date = date,
                                  month_start = month_start,
                                  label_year_start = label_year_start,
                                  break_min = NULL,
                                  open_left = FALSE,
                                  as_factor = as_factor)
}

## HAS_TESTS
#' Convert dates to multi-year periods
#'
#' Allocate dates to multi-year periods. The periods all have
#' the same width, which by default is 5 years.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' 
#' Periods are \code{width} years long.
#' They start on the first day of \code{month_start},
#' and end \code{width}-years-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' The location of the periods can be shifted
#' by using different values for \code{origin}.
#' 
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate periods,
#' including periods that not appear in the data.
#'
#' @inheritParams date_to_period_year
#' @param width The length, in whole years, of the periods.
#' Defaults to 5.
#' @param origin An integer. Defaults to 2000. 
#' 
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_period_custom}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_period_month}}.
#' Other functions for working with multi-year intervals are
#' \code{\link{date_to_age_group_multi}},
#' \code{\link{date_to_cohort_multi}},
#' and \code{\link{date_to_triangle_multi}}.
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods.
#'
#' @examples
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"))
#'
#' ## width is 10
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      width = 5)
#'
#' ## origin is 2001
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      origin = 2001)
#'
#' ## July to June
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      origin = 2001,
#'                      month_start = "Jul")
#'
#' ## return non-factor
#' date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                      as_factor = FALSE)
#' @export
date_to_period_multi <- function(date,
                                 width = 5,
                                 origin = 2000,
                                 month_start = "Jan",
                                 as_factor = TRUE) {
    date_to_period_or_cohort_multi(date = date,
                                   width = width,
                                   origin = origin,
                                   month_start = month_start,
                                   break_min = NULL,
                                   open_left = FALSE,
                                   as_factor = as_factor)
}

## HAS_TESTS
#' Convert dates to customized periods
#'
#' Allocate dates to periods with varying widths,
#' though all widths are measured in whole years.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' \code{breaks} is also vector of class \code{\link[base]{Date}},
#' or can be coerced to to one. \code{breaks} is
#' used to define the points where each period starts and finishes.
#' The dates in \code{breaks} must all be the first day of
#' the same month of the year. For instance,
#' \code{breaks} could consist of
#' the values \code{"2010-01-01"} and \code{"2017-01-01"},
#' but not \code{"2010-01-01"} and \code{"2017-01-02"},
#' or \code{"2010-01-01"} and \code{"2017-02-01"}.
#' The final period finishes
#' one day before the final break.
#' If, for instance, \code{breaks} has values \code{"2010-01-01"},
#' \code{"2017-01-01"}, and \code{"2021-01-01"}, then the first period
#' starts on 1 January 2010 and ends on 31 December 2016,
#' and the second period starts on 1 January 2017 and ends
#' on 31 December 2020.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate periods,
#' including periods that not appear in the data.
#'
#' @inheritParams date_to_period_year
#' @param breaks Dates defining starts and ends of periods.
#' 
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_quarter}},
#' and \code{\link{date_to_period_month}}.
#' Other functions for working with customized intervals are
#' \code{\link{date_to_age_group_custom}},
#' and \code{\link{date_to_cohort_custom}}.
#' See \code{\link{make_labels_period}} for the rules
#' on constructing labels for periods.
#'
#' @examples
#' ## periods start on 1 January
#' date_to_period_custom(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                       breaks = c("2000-01-01",
#'                                  "2019-01-01",
#'                                  "2026-01-01"))
#'
#' ## periods start on 1 March
#' date_to_period_custom(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                       breaks = c("2000-03-01",
#'                                  "2019-03-01",
#'                                  "2026-03-01"))
#'
#' ## return non-factor
#' date_to_period_custom(date = c("2024-03-27",
#'                                "2018-11-09",
#'                                "2021-03-02"),
#'                       breaks = c("2000-01-01",
#'                                  "2019-01-01",
#'                                  "2026-01-01"),
#'                       as_factor = FALSE)
#' @export
date_to_period_custom <- function(date,
                                  breaks,
                                  as_factor = TRUE) {
    date_to_period_or_cohort_custom(date = date,
                                    breaks = breaks,
                                    open_left = FALSE,
                                    as_factor = as_factor)
}

## HAS_TESTS
#' Convert dates to quarter periods
#'
#' Allocate dates to periods with periods one quarter
#' (ie three month) long. Q1 (the first quarter)
#' starts on 1 January
#' and ends on 31 March; Q2 starts on 1 April and ends on
#' 30 June; Q3 starts on 1 July and ends on 30 September;
#' Q4 starts on 1 October and ends on 31 December.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate periods,
#' including periods that not appear in the data.
#'
#' @inheritParams date_to_period_year
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_custom}},
#' and \code{\link{date_to_period_month}}.
#' Other functions for working with quarter intervals are
#' \code{\link{date_to_age_group_quarter}},
#' and \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#' See \code{\link{make_labels_period_quarter}} on the rules
#' for constructing labels for quarter periods.
#' @examples
#' date_to_period_quarter(date = c("2024-03-27",
#'                                 "2020-01-03",
#'                                 "2022-11-09"))
#' ## return non-factor
#' date_to_period_quarter(date = c("2024-03-27",
#'                                 "2020-01-03",
#'                                 "2022-11-09"),
#'                        as_factor = FALSE)
#' @export
date_to_period_quarter <- function(date,
                                   as_factor = TRUE) {
    date_to_period_or_cohort_quarter(date = date,
                                     break_min = NULL,
                                     open_left = FALSE,
                                     as_factor = as_factor)
}

## HAS_TESTS
#' Convert dates to month periods
#'
#' Allocate dates to periods with month-long
#' periods.
#'
#' \code{date} is a vector of class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate periods,
#' including periods that not appear in the data.
#'
#' @inheritParams date_to_period_year
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating periods are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_period_custom}},
#' and \code{\link{date_to_period_quarter}}.
#' Other functions for working with month intervals are
#' \code{\link{date_to_age_group_month}},
#' and \code{\link{date_to_cohort_month}},
#' and \code{\link{date_to_triangle_month}}.
#' See \code{\link{make_labels_period_month}} on the rules
#' for constructing labels for month periods.
#' @examples
#' date_to_period_month(date = c("2024-03-27",
#'                               "2020-01-03",
#'                               "2022-11-09"))
#' ## return non-factor
#' date_to_period_month(date = c("2024-03-27",
#'                               "2020-01-03",
#'                               "2022-11-09"),
#'                      as_factor = FALSE)
#' @export
date_to_period_month <- function(date,
                                 as_factor = TRUE) {
    date_to_period_or_cohort_month(date = date,
                                   break_min = NULL,
                                   open_left = FALSE,
                                   as_factor = as_factor)
}
