
## HAS_TESTS
#' Convert dates to one-year Lexis triangles
#'
#' Given dates when events occurred, together with dates of birth,
#' allocate the events to Lexis triangles.
#' All the Lexis triangles have widths
#' of one year, though the final upper triangle
#' typically has no age limit.
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
#' If \code{break_max} is \code{NULL}, \code{date_to_triangle_year}
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
#' The allocation of events to Lexis triangles becomes
#' tricky when the event and the date of birth share the same
#' month and day of the month (eg the event occurs on September
#' the 12th and the person's birthday is on
#' September the 12th.) Always allocating such
#' events to the lower triangle, or always allocating
#' them to the upper triangle, would lead to a slight
#' imbalance between upper and lower triangles. Instead,
#' if the dates match and it is first day of the month,
#' the event is allocated to \code{"Lower"}; if the dates match and
#' it is the second day of the month, the event is allocated to
#' \code{"Upper"}; if the dates match and it is the third day
#' of the month, the event is allocated to \code{"Lower"}; and so on.
#' See below for an example.
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
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The period starts on
#' the first day of this month.
#' @param as_factor Whether the return value is a factor.
#' Defaults to \code{TRUE}.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating Lexis triangles are
#' \code{\link{date_to_triangle_multi}},
#' \code{\link{date_to_triangle_fert}},
#' \code{\link{date_to_triangle_quarter}},
#' and \code{\link{date_to_triangle_month}}.
#' \code{date_to_triangle_year} is typically used in combination with
#' \code{\link{date_to_age_group_year}}
#' and \code{\link{date_to_period_year}}.
#'
#' @examples
#' date_to_triangle_year(date = c("2024-03-27",
#'                                "2022-11-09"),
#'                       dob = "2020-01-01")
#'
#' ## July to June
#' date_to_triangle_year(date = c("2024-03-27",
#'                                "2022-11-09"),
#'                       dob = "2020-01-01",
#'                       month_start = "Jul")
#'
#' ## open age group starts at 10 years
#' date_to_triangle_year(date = c("2017-03-27",
#'                                "2024-03-27"),
#'                       dob = "2010-01-01",
#'                       break_max = 10)
#' 
#' ## events and births occur on same month
#' ##  and day of month
#' date_to_triangle_year(date = c("2020-03-01",
#'                                "2020-03-02",
#'                                "2020-03-03",
#'                                "2020-03-04",
#'                                "2020-03-05"),
#'                       dob = c("2012-03-01",
#'                               "2012-03-02",
#'                               "2012-03-03",
#'                               "2012-03-04",
#'                               "2012-03-05"))
#'
#' ## return non-factor
#' date_to_triangle_year(date = c("2024-03-27",
#'                                "2022-11-09"),
#'                       dob = "2012-03-01",
#'                       as_factor = FALSE)
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


## HAS_TESTS
#' Convert dates to multi-year Lexis triangles
#'
#' Given dates when events occurred, together with dates of birth,
#' allocate the events to Lexis triangles.
#' All the Lexis triangles have the same width,
#' which by default is 5 years, though the final upper triangle
#' typically has no age limit.
#'
#' An event occurring during period \code{[t, t+n)} to
#' a person in age group \code{[a, a+n)} belongs to an
#' upper Lexis triangle if the person was already in age group
#' \code{[a, a+n)} at time \code{t}, and belongs to a
#' lower Lexis triangle if the person joins age group
#' \code{[a, a+n)} during period \code{[t, t+n)}.
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
#' group is \code{[break_max-width, break_max)} years.
#' 
#' If \code{break_max} is \code{NULL}, \code{date_to_triangle_multi}
#' derives a value, based on the highest age in the data,
#' and the value for \code{open_right}.
#' 
#' Periods start on the first day of \code{month_start},
#' and end \code{width}-years-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' The location of the periods can be shifted
#' by using different values for \code{origin}.
#'
#' The allocation of events to Lexis triangles becomes
#' tricky when the event and the date of birth share the same
#' month and day of the month (eg the event occurs on September
#' the 12th and the person's birthday is on
#' September the 12th), and the difference in years
#' is a multiple of \code{width}. Always allocating such
#' events to the lower triangle, or always allocating
#' them to the upper triangle, would lead to a slight
#' imbalance between upper and lower triangles. Instead,
#' if the dates match and it is first day of the month,
#' the event is allocated to \code{"Lower"}; if the dates match and
#' it is the second day of the month, the event is allocated to
#' \code{"Upper"}; if the dates match and it is the third day
#' of the month, the event is allocated to \code{"Lower"}; and so on.
#' See below for an example.
#'
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when they do not both appear
#' in the data.
#'
#' @inheritParams date_to_triangle_year
#' @param width The width in years of the periods and
#' age groups. A positive integer defaulting to 5.
#' @param origin An integer. Defaults to 2000. 
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating Lexis triangles are
#' \code{\link{date_to_triangle_year}},
#' \code{\link{date_to_triangle_fert}},
#' \code{\link{date_to_triangle_quarter}},
#' and \code{\link{date_to_triangle_month}}.
#' \code{date_to_triangle_multi} is typically used in combination with
#' \code{\link{date_to_age_group_multi}} and
#' \code{\link{date_to_period_multi}}.
#'
#' @examples
#' date_to_triangle_multi(date = c("2027-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2010-05-12")
#'
#' ## width is 10
#' date_to_triangle_multi(date = c("2027-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2010-05-12",
#'                        width = 10)
#'
#'
#' ## July to June
#' date_to_triangle_multi(date = c("2027-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2010-05-12",
#'                        month_start = "Jul")
#'
#' ## open age group starts at 10 years
#' date_to_triangle_multi(date = c("2027-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2003-05-12",
#'                        break_max = 10)
#' 
#' ## events and births occur on same month
#' ## and day of month, and years differ by
#' ## a multiple of width
#' date_to_triangle_multi(date = c("2020-03-01",
#'                                 "2020-03-02",
#'                                 "2020-03-03",
#'                                 "2020-03-04",
#'                                 "2020-03-05"),
#'                       dob = c("2010-03-01",
#'                               "2010-03-02",
#'                               "2010-03-03",
#'                               "2010-03-04",
#'                               "2010-03-05"))
#'
#' ## return non-factor
#' date_to_triangle_multi(date = c("2024-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2012-03-01",
#'                        as_factor = FALSE)
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
#' Convert dates to Lexis triangles used when measuring fertility
#'
#' Given dates when births occurred, together with the dates of birth
## of the mothers, allocate the births to Lexis triangles.
#' All the Lexis triangles have the same widths.
#'
#' An birth occurring during period \code{[t, t+n)} to
#' a woman in age group \code{[a, a+n)} belongs to an
#' upper Lexis triangle if the woman was already in age group
#' \code{[a, a+n)} at time \code{t}, and belongs to a
#' lower Lexis triangle if the woman joins age group
#' \code{[a, a+n)} during period \code{[t, t+n)}.
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
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
#' \code{break_min} and \code{break_max} specify
#' the range of ages over which reproduction
#' is assumed to occur. If, for instance,
#' \code{break_min} is \code{15} and \code{break_max}
#' is \code{50}, all births are assumed to
#' occur to women aged 15 to 49 (inclusive).
#'
#' Datasets sometimes contain a few births to mothers
#' younger than the assumed minimum age for reproduction,
#' or to mothers older than the assumed maximum age
#' for reproduction. Demographers often recode such births,
#' so that births to unexpectedly young mothers are
#' treated as occurring just above the minimum age
#' for reproduction, and births to unexpectedly old mothers
#' are treated as occurring just below the maximum
#' age for reproduction. This recoding can be justified
#' on the grounds that some of the original ages may have
#' been misreported, but it also alleviates any problems
#' with tabulations having small counts at extreme ages.
#' Recoding of mothers' ages outside the expected range
#' is controlled by parameters \code{recode_up}
#' and \code{recode_down}. The default
#' is for no recoding to occur.
#'
#' The allocation of a birth to a Lexis triangles becomes
#' tricky when children and mothers have the same birthday,
#' and when their dates of birth are some multiple of
#' \code{width} years apart. Always allocating such
#' births to the lower triangle, or always allocating
#' them to the upper triangle, would lead to a slight
#' imbalance between upper and lower triangles. Instead,
#' if the birthdays match and it is first day of the month,
#' the birth is allocated to \code{"Lower"}; if the birthdays match and
#' it is the second day of the month, the birth is allocated to
#' \code{"Upper"}; if the birthdays match and it is the third day
#' of the month, the birth is allocated to \code{"Lower"}; and so on.
#' See below for an example.
#'
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when they do not both appear
#' in the data.
#'
#' @inheritParams date_to_triangle_year
#' @param date Dates when births being measured occur.
#' @param dob Dates of birth of monthers.
#' @param width The width in years of the periods and
#' age groups. A positive integer defaulting to 5.
#' @param break_min An integer or \code{NULL}.
#' Defaults to 15.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 50.
#' @param recode_up If \code{TRUE}, births to women
#' aged less than \code{break_min} are treated as occurring to
#' women in the lowest repoductive age group.
#' @param recode_down If \code{TRUE}, births to women
#' aged \code{break_max} or more are treated as
#' occurring to women in the highest reproductive
#' age group.
#' @param origin An integer. Defaults to 2000. 
#'
#' 
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating Lexis triangles are
#' \code{\link{date_to_triangle_year}},
#' \code{\link{date_to_triangle_multi}},
#' \code{\link{date_to_triangle_quarter}},
#' and \code{\link{date_to_triangle_month}}.
#' \code{date_to_triangle_fert} is typically used in combination with
#' \code{\link{date_to_age_group_year}}
#' and \code{\link{date_to_period_year}}.
#'
#' @examples
#' date_to_triangle_fert(date = c("2024-03-27", "2022-11-09"),
#'                       dob = c("2001-03-21", "2000-07-13"))
#'
#' ## alternative values for 'width'
#' date_to_triangle_fert(date = c("2024-03-27", "2022-11-09"),
#'                       dob = c("2001-03-21", "2000-07-13"),
#'                       width = 10,
#'                       break_min = 20)
#' date_to_triangle_fert(date = c("2024-03-27", "2022-11-09"),
#'                       dob = c("2001-03-21", "2000-07-13"),
#'                       width = 1)
#'
#' ## births of children and mothers occur on
#' ## same month and day of month, and dates
#' ## are 4 * 5 years apart
#' date_to_triangle_fert(date = c("2020-03-01",
#'                                "2020-03-02",
#'                                "2020-03-03",
#'                                "2020-03-04",
#'                                "2020-03-05"),
#'                       dob = c("2000-03-01",
#'                               "2000-03-02",
#'                               "2000-03-03",
#'                               "2000-03-04",
#'                               "2000-03-05"))
#'
#' ## return non-factor
#' date_to_triangle_year(date = c("2024-03-27",
#'                                "2022-11-09"),
#'                       dob = "1996-03-01",
#'                       as_factor = FALSE)
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
#' Convert dates to quarter Lexis triangles
#'
#' Given dates when events occurred, together with dates of birth,
#' allocate the events to Lexis triangles.
#' All the Lexis triangles have widths
#' of one quarter (ie three months), though the final upper triangle
#' typically has no age limit.
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
#' If \code{break_max} is \code{NULL}, \code{date_to_triangle_quarter}
#' derives a value, based on the highest age in the data,
#' and the value for \code{open_right}.
#' 
#' The allocation of events to Lexis triangles becomes
#' tricky when the event and the date of birth share the same
#' day of the month and are a multiple of three months apart.
#' Always allocating such
#' events to the lower triangle, or always allocating
#' them to the upper triangle, would lead to a slight
#' imbalance between upper and lower triangles. Instead,
#' if the dates match and it is first day of the month,
#' the event is allocated to \code{"Lower"}; if the dates match and
#' it is the second day of the month, the event is allocated to
#' \code{"Upper"}; if the dates match and it is the third day
#' of the month, the event is allocated to \code{"Lower"}; and so on.
#' See below for an example.
#'
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when they do not both appear
#' in the data.
#'
#' @inheritParams date_to_triangle_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 400.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating Lexis triangles are
#' \code{\link{date_to_triangle_year}},
#' \code{\link{date_to_triangle_multi}},
#' \code{\link{date_to_triangle_fert}},
#' and \code{\link{date_to_triangle_month}}.
#' \code{date_to_triangle_quarter} is typically used in combination with
#' \code{\link{date_to_age_group_quarter}}
#' and \code{\link{date_to_period_quarter}}.
#'
#' @examples
#' date_to_triangle_quarter(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          dob = "2020-01-01")
#'
#'
#' ## open age group starts at 40 quarters
#' date_to_triangle_quarter(date = c("2017-03-27",
#'                                   "2024-03-27"),
#'                          dob = "2010-01-01",
#'                          break_max = 40)
#' 
#' ## events and births occur on same day of month
#' ## and are a multiple of three months apart
#' date_to_triangle_quarter(date = c("2020-04-01",
#'                                   "2020-04-02",
#'                                   "2020-04-03",
#'                                   "2020-04-04",
#'                                   "2020-04-05"),
#'                          dob = c("2019-01-01",
#'                                  "2019-01-02",
#'                                  "2019-01-03",
#'                                  "2019-01-04",
#'                                  "2019-01-05"))
#'
#' ## return non-factor
#' date_to_triangle_quarter(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          dob = "2012-03-01",
#'                          as_factor = FALSE)
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
#' Convert dates to month Lexis triangles
#'
#' Given dates when events occurred, together with dates of birth,
#' allocate the events to Lexis triangles.
#' All the Lexis triangles have widths
#' of one month , though the final upper triangle
#' typically has no age limit.
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
#' If \code{break_max} is \code{NULL}, \code{date_to_triangle_month}
#' derives a value, based on the highest age in the data,
#' and the value for \code{open_right}.
#' 
#' The allocation of events to Lexis triangles becomes
#' tricky when the event and the date of birth share the same
#' day of the month. Always allocating such
#' events to the lower triangle, or always allocating
#' them to the upper triangle, would lead to a slight
#' imbalance between upper and lower triangles. Instead,
#' if the dates match and it is first day of the month,
#' the event is allocated to \code{"Lower"}; if the dates match and
#' it is the second day of the month, the event is allocated to
#' \code{"Upper"}; if the dates match and it is the third day
#' of the month, the event is allocated to \code{"Lower"}; and so on.
#' See below for an example.
#'
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when they do not both appear
#' in the data.
#'
#' @inheritParams date_to_triangle_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 1200.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The return value has the same length as \code{date}.
#'
#' @seealso Other functions for creating Lexis triangles are
#' \code{\link{date_to_triangle_year}},
#' \code{\link{date_to_triangle_multi}},
#' \code{\link{date_to_triangle_fert}},
#' and \code{\link{date_to_triangle_quarter}}.
#' \code{date_to_triangle_month} is typically used in combination with
#' \code{\link{date_to_age_group_month}}
#' and \code{\link{date_to_period_month}}.
#'
#' @examples
#' date_to_triangle_month(date = c("2024-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2020-01-01")
#'
#'
#' ## open age group starts at 120 months
#' date_to_triangle_month(date = c("2017-03-27",
#'                                 "2024-03-27"),
#'                        dob = "2010-01-01",
#'                        break_max = 120)
#' 
#' ## events and births occur on same day of month
#' date_to_triangle_month(date = c("2020-02-01",
#'                                 "2020-02-02",
#'                                 "2020-02-03",
#'                                 "2020-02-04",
#'                                 "2020-01-05"),
#'                        dob = c("2019-01-01",
#'                                "2019-01-02",
#'                                "2019-01-03",
#'                                "2019-01-04",
#'                                "2019-01-05"))
#'
#' ## return non-factor
#' date_to_triangle_month(date = c("2024-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2012-03-01",
#'                        as_factor = FALSE)
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
