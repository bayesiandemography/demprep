
## HAS_TESTS
#' Convert dates to one-year Lexis triangles
#'
#' Given dates when events occurred, together with dates of birth,
#' allocate the events to Lexis triangles.
#' All the Lexis triangles have widths
#' of one year, though the final upper triangle
#' may have no age limit.
#'
#' An event occurring during period \code{[t, t+1)} to
#' a person in age group \code{[a, a+1)} belongs to an
#' upper Lexis triangle if the person was already in age group
#' \code{[a, a+1)} at time \code{t}, and belongs to a
#' lower Lexis triangle if the person joins age group
#' \code{[a, a+1)} during period \code{[t, t+1)}.
#'
#' Information on the Lexis triangle of an event, in addition
#' to the period and age group, allows that event to
#' be allocated to a birth cohort. The ability to allocate
#' events to birth cohorts is essential for demographic accounting.
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_max} and \code{open_right} are used to specify
#' the oldest age group.
#' When \code{break_max} is non-\code{NULL} and
#' \code{open_right} is \code{TRUE}, the oldest
#' age group is \code{[break_max, Inf)} years. When
#' \code{break_max} is non-\code{NULL} and 
#' \code{open_right} is \code{FALSE}, the oldest age
#' group is \code{[break_max-1, break_max)} years.
#' 
#' If \code{break_max} is \code{NULL}, \code{date_to_triangle}
#' derives a value, based on the highest age in the data,
#' and the value for \code{open_right}.
#' 
#' Periods start on the first day of \code{month_start},
#' and end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when they do not both appear
#' in the data.
#'
#' @param date Dates of events or measurements.
#' @param dob Dates of birth.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 100.
#' @param open_right Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#' @param as_factor Whether the return value is a factor.
#' Defaults to \code{TRUE}.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating Lexis triangles are
#' \code{\link{date_to_triangle_multi}},
#' \code{\link{date_to_triangle_quarter}},
#' and \code{\link{date_to_triangle_month}}.
#' Other functions for working with one-year intervals are
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_period_year}},
#' and \code{\link{date_to_cohort_year}}.
#'
#' @examples
#' date_to_triangle_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## July to June
#' date_to_triangle_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul")
#'
#' ## July to June, using the calendar year at
#' ## the end for the label
#' date_to_triangle_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#'
#' ## return non-factor
#' date_to_triangle_year(date = c("2024-03-27", "2022-11-09"),
#'                     as_factor = FALSE)
#' @export
date_to_triangle_year <- function(date,
                                  dob,
                                  break_max = 100,
                                  open_right = TRUE,
                                  month_start = "Jan",
                                  as_factor = TRUE) {
    date_to_triangle_multi(date = date,
                           dob = dob,
                           width = 1L,
                           break_max = break_max,
                           open_right = open_right,
                           origin = 2000L,
                           month_start = month_start,
                           as_factor = as_factor)
}


#' @rdname date_to_triangle
#' @export
date_to_triangle_fert <- function(date,
                                  dob,
                                  width = 5,
                                  break_min = 15,
                                  break_max = 50,
                                  recode_up = FALSE,
                                  recode_down = FALSE,
                                  origin = 2000,
                                  month_start = "Jan",
                                  as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    break_min <- demcheck::err_tdy_positive_integer_scalar(x = break_min,
                                                           name = "break_min",
                                                           null_ok = FALSE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = FALSE)
    demcheck::err_is_gt_scalar(x1 = break_max,
                               x2 = break_min,
                               name1 = "break_max",
                               name2 = "break_min")
    if ((break_max - break_min) %% width != 0L)
        stop(gettextf("difference between '%s' [%d] and '%s' [%d] not divisible by '%s' [%d]",
                      "break_max", break_max, "break_min", break_min, "width", width))
    demcheck::err_is_logical_flag(x = recode_up,
                                  name = "recode_up")
    demcheck::err_is_logical_flag(x = recode_down,
                                  name = "recode_down")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    is_lt_min <- age_years < break_min
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        if (recode_up) {
            dob_ymd$y[is_lt_min] <- date_ymd$y[is_lt_min] - break_min
            dob_ymd$m[is_lt_min] <- date_ymd$m[is_lt_min]
            dob_ymd$d[is_lt_min] <- date_ymd$d[is_lt_min]
        }
        else {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'break_min' is %d and 'recode_up' is FALSE"),
                          date[[i_lt_min]],
                          dob[[i_lt_min]],
                          age_years[[i_lt_min]],
                          break_min))
        }
    }
    is_ge_max <- age_years >= break_max
    i_ge_max <- match(TRUE, is_ge_max, nomatch = 0L)
    if (i_ge_max > 0L) {
        if (recode_down) {
            dob_ymd$y[is_ge_max] <- date_ymd$y[is_ge_max] - break_max + 1L
            dob_ymd$m[is_ge_max] <- date_ymd$m[is_ge_max]
            dob_ymd$d[is_ge_max] <- date_ymd$d[is_ge_max]
        }
        else {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'break_max' is %d and 'recode_down' is FALSE"),
                          date[[i_ge_max]],
                          dob[[i_ge_max]],
                          age_years[[i_ge_max]],
                          break_max))
        }
    }
    if ((i_lt_min > 0L) || (i_ge_max > 0L))
        dob <- as.Date(paste(dob_ymd$y,
                             dob_ymd$m,
                             dob_ymd$d,
                             sep = "-"))
    date_to_triangle_multi(date = date,
                           dob = dob,
                           width = width,
                           break_max = NULL,
                           open_right = FALSE,
                           origin = origin,
                           month_start = month_start,
                           as_factor = as_factor)    
}


## HAS_TESTS
#' @rdname date_to_triangle
#' @export
date_to_triangle_multi <- function(date,
                                   dob,
                                   width = 5,
                                   break_max = 100,
                                   open_right = TRUE,
                                   origin = 2000,
                                   month_start = "Jan",
                                   as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    if (!is.null(break_max) && !open_right)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    n <- length(date)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    i_month_within_period_date <- i_month_within_period(date_ymd = date_ymd,
                                                        width = width,
                                                        origin = origin,
                                                        month_start = month_start)
    i_month_within_period_dob <- i_month_within_period(date_ymd = dob_ymd,
                                                       width = width,
                                                       origin = origin,
                                                       month_start = month_start)
    is_lower_within_month <- is_lower_within_month(date_ymd = date_ymd,
                                                   dob_ymd = dob_ymd)
    is_lower <- ((i_month_within_period_date > i_month_within_period_dob)
        | ((i_month_within_period_date == i_month_within_period_dob)
            & is_lower_within_month))
    ans[is_lower] <- "Lower"
    may_have_ages_above_break_max <- !is.null(break_max) && open_right
    if (may_have_ages_above_break_max) {
        age_start <- age_completed_months_start_month(date_ymd = date_ymd,
                                                      dob_ymd = dob_ymd)
        is_open_upper <- !is.na(age_start) & (age_start >= 12L * break_max)
        ans[is_open_upper] <- "Upper"
    }
    if (as_factor) {
        ans <- factor(ans,
                      levels = c("Lower", "Upper"))
    }
    ans
}

## HAS_TESTS
#' @rdname date_to_triangle
#' @export
date_to_triangle_quarter <- function(date,
                                     dob,
                                     break_max = 400,
                                     open_right = TRUE,
                                     as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (!is.null(break_max) && !open_right)
        demcheck::err_exceeds_break_max_age(age = age_months,
                                            break_max = break_max,
                                            date = date,
                                            dob = dob,
                                            unit = "month")
    n <- length(date)
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    i_month_within_qu_date <- (date_ymd$m - 1L) %% 3L
    i_month_within_qu_dob <- (dob_ymd$m - 1L) %% 3L
    is_lower_within_month <- is_lower_within_month(date_ymd = date_ymd,
                                                   dob_ymd = dob_ymd)
    is_lower <- ((i_month_within_qu_date > i_month_within_qu_dob)
        | ((i_month_within_qu_date == i_month_within_qu_dob)
            & is_lower_within_month))
    ans[is_lower] <- "Lower"
    may_have_ages_above_break_max <- !is.null(break_max) && open_right
    if (may_have_ages_above_break_max) {
        age_start <- age_completed_months_start_month(date_ymd = date_ymd,
                                                      dob_ymd = dob_ymd)
        is_open_upper <- !is.na(age_start) & (age_start >= 3L * break_max)
        ans[is_open_upper] <- "Upper"
    }
    if (as_factor) {
        ans <- factor(ans,
                      levels = c("Lower", "Upper"))
    }
    ans
}

## HAS_TESTS
#' @rdname date_to_triangle
#' @export
date_to_triangle_month <- function(date, dob,
                                   break_max = 1200,
                                   open_right = TRUE,
                                   as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (!is.null(break_max) && !open_right)
        demcheck::err_exceeds_break_max_age(age = age_months,
                                            break_max = break_max,
                                            date = date,
                                            dob = dob,
                                            unit = "month")
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    n <- length(date)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    is_lower <- is_lower_within_month(date_ymd = date_ymd,
                                      dob_ymd = dob_ymd)
    ans[is_lower] <- "Lower"
    may_have_ages_above_break_max <- !is.null(break_max) && open_right
    if (may_have_ages_above_break_max) {
        age_start <- age_completed_months_start_month(date_ymd = date_ymd,
                                                      dob_ymd = dob_ymd)
        is_open_upper <- !is.na(age_start) & (age_start >= break_max)
        ans[is_open_upper] <- "Upper"
    }
    if (as_factor) {
        ans <- factor(ans,
                      levels = c("Lower", "Upper"))
    }
    ans
}
