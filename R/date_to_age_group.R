
#' Convert dates to age groups
#'
#' Convert precise dates into broader age groups,
#' in a dataset describing individual people or events.
#'
#' Raw data on individual people or events often do not
#' give people's ages but instead give the
#' date when the data was collected or the event occurred,
#' plus the person's date of birth. Functions \code{date_to_age_group_month},
#' \code{date_to_age_group_quarter}, and \code{date_to_age_group_year}
#' can be used to calculate ages from information on dates.
#' The functions focus on the typical cases, rather than try to
#' cover every possible input or output.
#' 
#' With one exception, the age groups constructed
#' by \code{date_to_age_group_month} always have a
#' width of one month, and the age groups constructed
#' by \code{date_to_age_group_year} always have a
#' width of one quarter.  The exception is the
#' highest age group, which is typically 'open', i.e. has
#' no upper limit. The age groups constructed by \code{date_to_age_group_year}
#' by default have a width of one year. However, setting \code{width}
#' to a number other than 1 will give wider intervals.
#' For intervals with varying lengths, use \code{breaks}.
#' See below for examples.
#'
#' The \code{age_open} argument specifies the start of
#' the open age group. By default this is 1200 months
#' for \code{date_to_age_group_month},
#' 400 quarters for \code{date_to_age_group_quarter}, 
#' and for \code{date_to_age_group_year}.  If you don't want an
#' open age group, set \code{age_open} to \code{NULL}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' By default, the functions returns a factor. The levels of this
#' factor contain all intermediate age groups between the lowest
#' and highest, even if these were not found in the data.
#' To return a character vector instead, set \code{as_factor}
#' to \code{FALSE}.
#' 
#' @param date The date of the event or measurement.
#' @param dob The individual's date of birth.
#' @param age_open The minimum age for the open age group 
#' age, measured in months, quarters, or years.
#' @param width The width, in years, of the age groups.
#' A positive integer.
#' @param breaks The points dividing the intervals. 
#' @param as_factor Whether the return value should be a factor
#' Defaults to \code{TRUE}.
#' 
#' @return If \code{as_factor} is \code{TRUE}, a factor;
#' otherwise a character vector. The length equals the length
#' of \code{date} or the length of \code{dob}, whichever
#' is greater.
#'
#' @seealso To turn dates into periods, cohorts, or Lexis triangles,
#' use functions such as \code{date_to_period_year},
#' \code{date_to_cohort_year}, and \code{date_to_triangle_year}.
#' If none of these functions can do the job, try \code{\link[base:cut}},
#' possibly in combination with \code{\link{make_age_labels}}.
#'
#' WHAT ABOUT LOWEST AGE GROUP? HOW TO SPECIFY OPEN AGE GROUP WHEN
#' USING BREAKS ARG?
#' 
#' @examples
#' 
#'
#' @rdname date_to_age_group
#' @export
NULL

#' @rdname date_to_age_group
#' @export
date_to_age_group <- function(date, dob,
                              min = 0, max = 100, width = 1,
                              breaks = NULL,
                              open_right = TRUE,
                              as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    has_breaks <- !is.null(breaks)
    if (has_breaks)
        breaks <- err_tdy_breaks(breaks)
    else {
        l <- err_tdy_min_max(min = min,
                             max = max)
        min <- l$min
        max <- l$max
        width <- err_tdy_width(width = width,
                               min = min,
                               max = max)
    }
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age <- date_to_age_completed_months(date = date,
                                        dob = dob)
    err_age_ge_min(age = age,
                   min = min,
                   date = date,
                   dob = dob,
                   unit = "year")
    if (!open_right) {
        err_age_lt_max(age = age,
                       max = max,
                       date = date,
                       dob = dob,
                       unit = "year")
    } 
    age_to_label(age = age,
                 min = min,
                 max = max,
                 width = width,
                 breaks = breaks,
                 open_left = FALSE,
                 open_right = open_right,
                 as_factor = as_fractor)
}


#' @rdname date_to_age_group
#' @export
date_to_age_group_month <- function(date, dob,
                                    min = 0, max = 1200,
                                    open_right = TRUE,
                                    as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    l <- err_tdy_min_max(min = min,
                         max = max)
    min <- l$min
    max <- l$max
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age <- date_to_age_completed_months(date = date,
                                        dob = dob)
    err_age_ge_min(age = age,
                   min = min,
                   date = date,
                   dob = dob,
                   unit = "month")
    if (!open_right) {
        err_age_lt_max(age = age,
                       max = max,
                       date = date,
                       dob = dob,
                       unit = "month")
    } 
    age_to_label_month(age = age, 
                       min = min,
                       max = max,
                       open_left = FALSE,
                       open_right = open_right,
                       as_factor = as_factor)
}


#' @rdname date_to_age_group
#' @export
date_to_age_group_quarter <- function(date, dob,
                                      min = 0, max = 400,
                                      open_right = TRUE,
                                      as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    l <- err_tdy_min_max(min = min,
                         max = max)
    min <- l$min
    max <- l$max
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age <- date_to_age_completed_months(date = date,
                                        dob = dob)
    err_age_ge_min(age = age,
                   min = min,
                   date = date,
                   dob = dob,
                   unit = "quarter")
    if (!open_right) {
        err_age_lt_max(age = age,
                       max = max,
                       date = date,
                       dob = dob,
                       unit = "quarter")
    } 
    age_to_label_quarter(age = age, 
                         min = min,
                         max = max,
                         open_left = FALSE,
                         open_right = open_right,
                         as_factor = as_factor)
}
