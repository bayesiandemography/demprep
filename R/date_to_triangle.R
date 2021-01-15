
## HAS_TESTS
#' Convert dates to one-year Lexis triangles
#'
#' Based on an age-time plan where age groups and periods both have
#' widths of one year, use dates of events and dates of birth
#' to allocate events to Lexis triangles.
#'
#' The allocation of an event to a Lexis triangle depends
#' on the timing of the event and on the timing of changes
#' in age group. A person moves up into a new age group
#' once during each period. An event is allocated
#' to an upper Lexis triangle if the event occurs
#' before the move to the new age group.
#' An event is allocated to a lower Lexis triangle
#' if the event occurs with or after the move to the
#' new age group.
#'
#' Consider, for instance, events occurring to a person
#' who was born on 28 March in an age-time plan
#' where periods start on 1 January. 
#' If an event occurs on any day
#' from 1 January to 27 March (inclusive), then
#' \code{date_to_triangle_year} allocates the event
#' to an upper Lexis triangle.
#' If an event occurs on
#' any day from 28 March to 31 December (inclusive), then
#' \code{date_to_triangle_year} allocates the event
#' to a lower Lexis triangle.
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_max} and \code{open_last} are used to specify
#' the oldest age group.
#' When \code{break_max} is non-\code{NULL} and
#' \code{open_last} is \code{TRUE}, the oldest
#' age group is \code{[break_max, Inf)} years. When
#' \code{break_max} is non-\code{NULL} and 
#' \code{open_last} is \code{FALSE}, the oldest age
#' group is \code{[break_max-1, break_max)} years.
#' 
#' If \code{break_max} is \code{NULL}, then
#' \code{date_to_triangle_year} derives a value
#' for \code{break_max} based on the highest age in the data
#' and on the value for \code{open_last}.
#' 
#' Periods start on the first day of \code{month_start},
#' and end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December.
#'
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when \code{"Lower"}
#' and \code{"Upper"} do not both appear
#' in the data.
#'
#' @param date Dates of events.
#' @param dob Dates of birth.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 100.
#' @param open_last Whether the final age group
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
#' \code{\link{date_to_triangle_births}},
#' \code{\link{date_to_triangle_quarter}},
#' and \code{\link{date_to_triangle_month}}.
#' \code{date_to_triangle_year} is typically used in combination with
#' \code{\link{date_to_age_year}}
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
#' ## events occurring to people born during the
#' ## period in question are always allocated to
#' ## lower Lexis triangles
#' date_to_triangle_year(date = c("2020-03-19",
#'                                "2020-06-18"),
#'                       dob = c("2020-03-01",
#'                               "2020-06-18"))
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
                                  open_last = TRUE,
                                  month_start = "Jan",
                                  as_factor = TRUE) {
    date_to_triangle_multi(date = date,
                           dob = dob,
                           width = 1L,
                           break_max = break_max,
                           open_last = open_last,
                           origin = 2000L,
                           month_start = month_start,
                           as_factor = as_factor)
}


## HAS_TESTS
#' Convert dates to multi-year Lexis triangles
#'
#' Based on an age-time plan where age groups and periods have
#' identical widths, use dates of events and dates of birth
#' to allocate events to Lexis triangles.
#'
#' The allocation of an event to a Lexis triangle depends
#' on the timing of the event and on the timing of change
#' in age group. A person moves up into a new age group
#' once during each period. An event is allocated
#' to an upper Lexis triangle if the event occurs
#' before the move to the new age group.
#' An event is allocated to a lower Lexis triangle
#' if the event occurs with or after the move to the
#' new age group.
#'
#' Consider, for instance, events occurring to a person
#' who was born on 28 March 2001, in an age-time plan
#' where periods start on 1 January 2000, 1 January 2005, etc.
#' \code{date_to_triangle_multi} allocates events
#' to Lexis triangles as follows:
#' \tabular{ll}{
#'   \emph{Date of event} \tab \emph{Lexis triangle} \cr
#'   28 March 2001 to 31 December 2004 \tab \code{"Lower"} \cr
#'   1 January 2005 to 27 March 2006 \tab \code{"Upper"} \cr
#'   28 March 2006 to 31 December 2009 \tab \code{"Lower"} \cr
#'   1 January 2010 to 27 March 2011 \tab \code{"Upper"} \cr
#'   etc \tab etc
#' }
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_max} and \code{open_last} are used to specify
#' the oldest age group.
#' When \code{break_max} is non-\code{NULL} and
#' \code{open_last} is \code{TRUE}, the oldest
#' age group is \code{[break_max, Inf)} years. When
#' \code{break_max} is non-\code{NULL} and 
#' \code{open_last} is \code{FALSE}, the oldest age
#' group is \code{[break_max-width, break_max)} years.
#' 
#' If \code{break_max} is \code{NULL}, then
#' \code{date_to_triangle_multi} derives a value for
#' \code{break_max} based on the highest age in the data
#' and on the value for \code{open_last}.
#' 
#' Periods start on the first day of \code{month_start},
#' and end \code{width}-years-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December.
#'
#' The location of the periods can be shifted
#' by using different values for \code{origin}.
#'
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when \code{"Lower"}
#' and \code{"Upper"} do not both appear
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
#' \code{\link{date_to_triangle_births}},
#' \code{\link{date_to_triangle_quarter}},
#' and \code{\link{date_to_triangle_month}}.
#' \code{date_to_triangle_multi} is typically used in combination with
#' \code{\link{date_to_age_multi}} and
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
#' ## periods start in 2021, 2026, 2031, etc
#' date_to_triangle_multi(date = c("2027-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2010-05-12",
#'                        origin = 2001)
#'
#' ## open age group starts at 10 years
#' date_to_triangle_multi(date = c("2027-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2003-05-12",
#'                        break_max = 10)
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
                                   open_last = TRUE,
                                   origin = 2000,
                                   month_start = "Jan",
                                   as_factor = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    ## calculate age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## if has upper limit, check that all
    ## ages less than limit
    if (!is.null(break_max) && !open_last)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    ## prepare to assign triangles
    ans <- rep(NA_character_, times = length(date))
    date_start_period <- rollback_multi(date = date,
                                        width = width,
                                        origin = origin,
                                        month_start = month_start)
    ## (subtract one day so that events occurring to people who
    ## attain age on first day of period are coded as "Lower")
    date_before_period <- date_start_period - 1L
    age_months_before_period <- age_completed_months(date = date_before_period,
                                                     dob = dob)
    age_years_before_period <- age_months_before_period %/% 12L
    ## assign triangles - born during period
    born_during_period <- (!is.na(dob)
        & !is.na(date_before_period)
        & (dob > date_before_period)) ## includes first day of period
    ans[born_during_period] <- "Lower"
    ## assign triangles - in open age group before start of period
    if (open_last && !is.null(break_max)) {
        in_open <- (!is.na(age_years_before_period)
            & (age_years_before_period >= break_max))
    }
    else
        in_open <- rep(FALSE, length = length(ans))
    ans[in_open] <- "Upper"
    ## assign triangles - remaining age groups
    i_age_before <- age_years_before_period %/% width
    i_age_now <- age_years %/% width
    is_unassigned <- (!born_during_period
        & !in_open
        & !is.na(i_age_before)
        & !is.na(i_age_now))
    diff_age <- i_age_now - i_age_before
    if (any(is_unassigned & !(diff_age %in% c(0L, 1L))))
        stop("invalid value for 'diff_age'")
    is_lower <- is_unassigned & (i_age_now == i_age_before + 1L)
    is_upper <- is_unassigned & !is_lower
    ans[is_lower] <- "Lower"
    ans[is_upper] <- "Upper"
    ## return result
    if (as_factor) {
        ans <- factor(ans,
                      levels = c("Lower", "Upper"))
    }
    ans
}


## HAS_TESTS
#' Convert dates to Lexis triangles used when measuring fertility
#'
#' Use dates when births occurred and dates of birth
#' of parents to allocate births to Lexis triangles.
#'
#' The allocation of a birth to a Lexis triangle depends
#' on the timing of the birth and on the timing of changes
#' in the parent's age group. A person moves up into a new age group
#' once during each period. A birth is allocated
#' to an upper Lexis triangle if the birth occurs
#' before the parent's move to the new age group.
#' A birth is allocated to a lower Lexis triangle
#' if the birth occurs with or after the parent's
#' move to the new age group.
#'
#' Consider, for instance, births occurring to a parent
#' who was born on 28 March 2001, in an age-time plan
#' where periods start on 1 January 2000, 1 January 2005, etc.
#' \code{date_to_triangle_births} allocates births
#' to Lexis triangles as follows:
#' \tabular{ll}{
#'   \emph{Date} \tab \emph{Lexis triangle} \cr
#'   28 March 2001 to 31 December 2004 \tab \code{"Lower"} \cr
#'   1 January 2005 to 27 March 2006 \tab \code{"Upper"} \cr
#'   28 March 2006 to 31 December 2009 \tab \code{"Lower"} \cr
#'   1 January 2010 to 27 March 2011 \tab \code{"Upper"} \cr
#'   etc \tab etc
#' }
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
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when \code{"Lower"}
#' and \code{"Upper"} do not both appear
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
#' \code{date_to_triangle_births} is typically used in combination with
#' \code{\link{date_to_age_year}}
#' and \code{\link{date_to_period_year}}.
#'
#' @examples
#' date_to_triangle_births(date = c("2024-03-27", "2022-11-09"),
#'                       dob = c("2001-03-21", "2000-07-13"))
#'
#' ## alternative values for 'width'
#' date_to_triangle_births(date = c("2024-03-27", "2022-11-09"),
#'                       dob = c("2001-03-21", "2000-07-13"),
#'                       width = 10,
#'                       break_min = 20)
#' date_to_triangle_births(date = c("2024-03-27", "2022-11-09"),
#'                       dob = c("2001-03-21", "2000-07-13"),
#'                       width = 1)
#'
#' ## return non-factor
#' date_to_triangle_year(date = c("2024-03-27",
#'                                "2022-11-09"),
#'                       dob = "1996-03-01",
#'                       as_factor = FALSE)
#' @export
date_to_triangle_births <- function(date,
                                    dob,
                                    width = 5,
                                    break_min = 15,
                                    break_max = 50,
                                    recode_up = FALSE,
                                    recode_down = FALSE,
                                    origin = 2000,
                                    month_start = "Jan",
                                    as_factor = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
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
    demcheck::err_gt_scalar(x1 = break_max,
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
    ## calculate age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## recode ages outside 'break_min', 'break_max', if requested
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
    ## hand over to 'date_to_triangle_multi'
    date_to_triangle_multi(date = date,
                           dob = dob,
                           width = width,
                           break_max = NULL,
                           open_last = FALSE,
                           origin = origin,
                           month_start = month_start,
                           as_factor = as_factor)    
}

## HAS_TESTS
#' Convert dates to quarter-length Lexis triangles
#'
#' Based on an age-time plan where age groups and periods both have
#' widths of one quarter (ie three months),
#' use dates of events and dates of birth
#' to allocate events to Lexis triangles.
#'
#' The allocation of an event to a Lexis triangle depends
#' on the timing of the event and on the timing of changes
#' in age group. A person moves up into a new age group
#' once during each period. An event is allocated
#' to an upper Lexis triangle if the event occurs
#' before the move to the new age group.
#' An event is allocated to a lower Lexis triangle
#' if the event occurs with or after the move to the
#' new age group.
#'
#' Consider, for instance, events occurring to a person
#' who was born on 3 March. \code{date_to_triangle_quarter}
#' allocates events to Lexis triangles as follows:
#' \tabular{ll}{
#'   \emph{Date of event} \tab \emph{Lexis triangle} \cr
#'   3 March to 31 March \tab \code{"Lower"} \cr
#'   1 April to 2 June \tab \code{"Upper"} \cr
#'   3 June to 31 September \tab \code{"Lower"} \cr
#'   1 October to 2 December \tab \code{"Upper"} \cr
#'   3 December to 31 December \tab \code{"Lower"} \cr
#'   etc \tab etc
#' }
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_max} and \code{open_last} are used to specify
#' the oldest age group.
#' When \code{break_max} is non-\code{NULL} and
#' \code{open_last} is \code{TRUE}, the oldest
#' age group is \code{[break_max, Inf)} years. When
#' \code{break_max} is non-\code{NULL} and 
#' \code{open_last} is \code{FALSE}, the oldest age
#' group is \code{[break_max-1, break_max)} years.
#' 
#' If \code{break_max} is \code{NULL}, then
#' \code{date_to_triangle_quarter}
#' derives a value for \code{break_max}
#' based on the highest age in the data,
#' and the value for \code{open_last}.
#' 
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when \code{"Lower"}
#' and \code{"Upper"} do not both appear
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
#' \code{\link{date_to_triangle_births}},
#' and \code{\link{date_to_triangle_month}}.
#' \code{date_to_triangle_quarter} is typically used in combination with
#' \code{\link{date_to_age_quarter}}
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
#' ## return non-factor
#' date_to_triangle_quarter(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          dob = "2012-03-01",
#'                          as_factor = FALSE)
#' @export
date_to_triangle_quarter <- function(date,
                                     dob,
                                     break_max = 400,
                                     open_last = TRUE,
                                     as_factor = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    ## calculate age in months and quarters
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    ## if has upper limit, check that all
    ## ages less than limit
    if (!is.null(break_max) && !open_last) {
        demcheck::err_lt_break_max_age(age = age_quarters,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "month")
    }
    ## prepare to assign triangles
    ans <- rep(NA_character_, times = length(date))
    date_start_quarter <- rollback_quarter(date)
    ## (subtract one day so that events occurring to people who
    ## attain age on first day of quarter are coded as "Lower")
    date_before_quarter <- date_start_quarter - 1L
    age_months_before_quarter <- age_completed_months(date = date_before_quarter,
                                                      dob = dob)
    age_quarters_before_quarter <- age_months_before_quarter %/% 3L
    ## assign triangles - born during quarter
    born_during_quarter <- (!is.na(dob)
        & !is.na(date_before_quarter)
        & (dob > date_before_quarter)) ## includes first day of quarter
    ans[born_during_quarter] <- "Lower"
    ## assign triangles - in open age group before start of quarter
    if (open_last && !is.null(break_max)) {
        in_open <- (!is.na(age_quarters_before_quarter)
            & (age_quarters_before_quarter >= break_max))
    }
    else
        in_open <- rep(FALSE, length = length(ans))
    ans[in_open] <- "Upper"
    ## assign triangles - remaining age groups
    is_unassigned <- (!born_during_quarter
        & !in_open
        & !is.na(age_quarters_before_quarter)
        & !is.na(age_quarters))
    diff_age <- age_quarters - age_quarters_before_quarter
    if (any(is_unassigned & !(diff_age %in% c(0L, 1L))))
        stop("invalid value for 'diff_age'")
    is_lower <- is_unassigned & (age_quarters == age_quarters_before_quarter + 1L)
    is_upper <- is_unassigned & !is_lower
    ans[is_lower] <- "Lower"
    ans[is_upper] <- "Upper"
    ## return result
    if (as_factor) {
        ans <- factor(ans,
                      levels = c("Lower", "Upper"))
    }
    ans
}

## HAS_TESTS
#' Convert dates to month-length Lexis triangles
#'
#' Based on an age-time plan where age groups and periods
#' both have widths of one month,
#' use dates of events and dates of birth
#' to allocate events to Lexis triangles.
#'
#' The allocation of an event to a Lexis triangle depends
#' on the timing of the event and on the timing of changes
#' in age group. A person moves up into a new age group
#' once during each period. An event is allocated
#' to an upper Lexis triangle if the event occurs
#' before the move to the new age group.
#' An event is allocated to a lower Lexis triangle
#' if the event occurs with or after the move to the
#' new age group.
#'
#' Consider, for instance, events occurring to a person
#' who was born on 11 March. \code{date_to_triangle_month}
#' allocates events to Lexis triangles as follows:
#' \tabular{ll}{
#'   \emph{Date of event} \tab \emph{Lexis triangle} \cr
#'   11 March to 31 March \tab \code{"Lower"} \cr
#'   1 April to 10 April \tab \code{"Upper"} \cr
#'   11 April to 30 April \tab \code{"Lower"} \cr
#'   1 May to 10 May \tab \code{"Upper"} \cr
#'   11 May to 31 May \tab \code{"Lower"} \cr
#'   etc \tab etc
#' }
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_max} and \code{open_last} are used to specify
#' the oldest age group.
#' When \code{break_max} is non-\code{NULL} and
#' \code{open_last} is \code{TRUE}, the oldest
#' age group is \code{[break_max, Inf)} years. When
#' \code{break_max} is non-\code{NULL} and 
#' \code{open_last} is \code{FALSE}, the oldest age
#' group is \code{[break_max-1, break_max)} years.
#' 
#' If \code{break_max} is \code{NULL},
#' then \code{date_to_triangle_month}
#' derives a value for \code{break_max} based on
#' the highest age in the data,
#' and the value for \code{open_last}.
#' 
#' When \code{as_factor} is \code{TRUE}, the levels of
#' the factor includes both \code{"Lower"} and
#' \code{"Upper"}, even when \code{"Lower"} and
#' \code{"Upper"} do not both appear
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
#' \code{\link{date_to_triangle_births}},
#' and \code{\link{date_to_triangle_quarter}}.
#' \code{date_to_triangle_month} is typically used in combination with
#' \code{\link{date_to_age_month}}
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
#' ## return non-factor
#' date_to_triangle_month(date = c("2024-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2012-03-01",
#'                        as_factor = FALSE)
#' @export
date_to_triangle_month <- function(date, dob,
                                   break_max = 1200,
                                   open_last = TRUE,
                                   as_factor = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    ## calculate age in months
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    ## if has upper limit, check that all
    ## ages less than limit
    if (!is.null(break_max) && !open_last) {
        demcheck::err_lt_break_max_age(age = age_months,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "month")
    }
    ## prepare to assign triangles
    ans <- rep(NA_character_, times = length(date))
    date_start_month <- rollback_month(date)
    ## (subtract one day so that events occurring to people who
    ## attain age on first day of month are coded as "Lower")
    date_before_month <- date_start_month - 1L
    age_months_before_month <- age_completed_months(date = date_before_month,
                                                    dob = dob)
    ## assign triangles - born during month
    born_during_month <- (!is.na(dob)
        & !is.na(date_before_month)
        & (dob > date_before_month)) ## includes first day of month
    ans[born_during_month] <- "Lower"
    ## assign triangles - in open age group before start of month
    if (open_last && !is.null(break_max)) {
        in_open <- (!is.na(age_months_before_month)
            & (age_months_before_month >= break_max))
    }
    else
        in_open <- rep(FALSE, length = length(ans))
    ans[in_open] <- "Upper"
    ## assign triangles - remaining age groups
    is_unassigned <- (!born_during_month
        & !in_open
        & !is.na(age_months_before_month)
        & !is.na(age_months))
    diff_age <- age_months - age_months_before_month
    if (any(is_unassigned & !(diff_age %in% c(0L, 1L))))
        stop("invalid value for 'diff_age'")
    is_lower <- is_unassigned & (age_months == age_months_before_month + 1L)
    is_upper <- is_unassigned & !is_lower
    ans[is_lower] <- "Lower"
    ans[is_upper] <- "Upper"
    ## return result
    if (as_factor) {
        ans <- factor(ans,
                      levels = c("Lower", "Upper"))
    }
    ans
}
