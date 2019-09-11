
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
#' If none of these functions can do the job, try \code{\link[base]{cut}},
#' possibly in combination with \code{\link{make_age_labels}}.
#'
#' WHAT ABOUT LOWEST AGE GROUP? HOW TO SPECIFY OPEN AGE GROUP WHEN
#' USING BREAKS ARG?
#'
#'
#' If as_factor is TRUE, and date or dob have NAs, then the
#' levels of the answer will include NA.
#' 
#' @examples
#' @name date_to_age_group
NULL

## Assume every (non-NA) record belongs to appropriate age range.
## Do not use these functions for filtering.
## Leave NAs in because these records might later have age imputed.

## 'age_max' can be Inf - except for lifetab. In that case, the levels run from 0 to maximum observed age.

## HAS_TESTS
#' @rdname date_to_age_group
#' @export
date_to_age_group_year <- function(date, dob,
                                   age_max = 100,
                                   open_right = TRUE,
                                   as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- demcheck::err_tdy_positive_integer_scalar(x = age_max,
                                                         name = "age_max",
                                                         inf_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    if (!open_right)
        demcheck::err_lt_age_max(age = age_years,
                                 age_max = age_max,
                                 date = date,
                                 dob = dob,
                                 unit = "year")
    breaks <- make_breaks_integer_year(age = age_years,
                                       age_max = age_max,
                                       open_right = open_right)
    include_na <- any(is.na(age_years))
    labels <- make_labels_age_group_year(breaks = breaks,
                                         open_left = FALSE,
                                         open_right = open_right,
                                         include_na = include_na)
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}    

## HAS_TESTS
#' @rdname date_to_age_group
#' @export
date_to_age_group_multi <- function(date, dob,
                                    width = 5,
                                    age_max = 100,
                                    open_right = TRUE,
                                    as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    age_max <- demcheck::err_tdy_positive_integer_scalar(x = age_max,
                                                         name = "age_max",
                                                         inf_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    demcheck::err_is_multiple_of(x1 = age_max,
                                 x2 = width,
                                 name1 = "age_max",
                                 name2 = "width",
                                 inf_ok = TRUE)
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    if (!open_right)
        demcheck::err_lt_age_max(age = age_years,
                                 age_max = age_max,
                                 date = date,
                                 dob = dob,
                                 unit = "year")    
    breaks <- make_breaks_integer_multi(age = age_years,
                                        width = width,
                                        age_max = age_max,
                                        open_right = open_right)
    include_na <- any(is.na(age_years))
    labels <- make_labels_age_group_year(breaks = breaks,
                                         open_left = FALSE,
                                         open_right = open_right,
                                         include_na = include_na)
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}

## HAS_TESTS
#' @rdname date_to_age_group
#' @export
date_to_age_group_lifetab <- function(date, dob,
                                      age_max = 100,
                                      as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- demcheck::err_tdy_positive_integer_scalar(x = age_max,
                                                         name = "age_max",
                                                         inf_ok = FALSE)
    demcheck::err_is_multiple_of_n(x = age_max,
                                   name = "age_max",
                                   n = 5L,
                                   inf_ok = FALSE)
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    breaks <- make_breaks_integer_lifetab(age_max)
    include_na <- any(is.na(age_years))
    labels <- make_labels_age_group_year(breaks = breaks,
                                         open_left = FALSE,
                                         open_right = TRUE,
                                         include_na = include_na)
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}

## HAS_TESTS
#' @rdname date_to_age_group
#' @export
date_to_age_group_fert <- function(date, dob,
                                   age_min = 15,
                                   age_max = 50,
                                   width = 5,
                                   recode_up = FALSE,
                                   recode_down = FALSE,
                                   as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    age_min <- demcheck::err_tdy_positive_integer_scalar(x = age_min,
                                                         name = "age_min",
                                                         inf_ok = FALSE)
    age_max <- demcheck::err_tdy_positive_integer_scalar(x = age_max,
                                                         name = "age_max",
                                                         inf_ok = FALSE)
    demcheck::err_is_gt_scalar(x1 = age_max,
                               x2 = age_min,
                               name1 = "age_max",
                               name2 = "age_min")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if ((age_max - age_min) %% width != 0L)
        stop(gettextf("difference between '%s' [%d] and '%s' [%d] not divisible by '%s' [%d]",
                      "age_max", age_max, "age_min", age_min, "width", width))
    demcheck::err_is_logical_flag(x = recode_up,
                                  name = "recode_up")
    demcheck::err_is_logical_flag(x = recode_down,
                                  name = "recode_down")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    breaks <- seq(from = age_min,
                  to = age_max,
                  by = width)
    is_lt_min <- age_years < age_min
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        if (recode_up)
            age_years[is_lt_min] <- age_min
        else {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'age_min' is %d and 'recode_up' is FALSE"),
                                date[[i_lt_min]],
                                dob[[i_lt_min]],
                                age_years[[i_lt_min]],
                                age_min))
        }
    }
    is_ge_max <- age_years >= age_max
    i_ge_max <- match(TRUE, is_ge_max, nomatch = 0L)
    if (i_ge_max > 0L) {
        if (recode_down)
            age_years[is_ge_max] <- age_max - 1L
        else {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'age_max' is %d and 'recode_down' is FALSE"),
                          date[[i_ge_max]],
                          dob[[i_ge_max]],
                          age_years[[i_ge_max]],
                          age_max))
        }
    }
    include_na <- any(is.na(age_years))
    labels <- make_labels_age_group_year(breaks = breaks,
                                         open_left = FALSE,
                                         open_right = FALSE,
                                         include_na = include_na)
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}


#' @rdname date_to_age_group
#' @export
date_to_age_group_custom <- function(date, dob,
                                     breaks = NULL,
                                     open_right = TRUE,
                                     as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    breaks <- demcheck::err_tdy_breaks_age(x = breaks,
                                 name = "breaks")
    demcheck::err_is_logical_flag(x = open_right,
                        name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    is_lt_min <- age_years < breaks[[1L]]
    if (any(is_lt_min)) {
        i <- match(TRUE, is_lt_min)
        stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                            "but minimum value for '%s' is %d"),
                      date[[i]],
                      dob[[i]],
                      age[[i]],
                      "breaks",
                      breaks[[1L]]))
    }
    if (!open_right)
        demcheck::err_exceeds_age_max(age = age_years,
                            age_max = age_max,
                            date = date,
                            dob = dob,
                            unit = "year")    
    include_na <- any(is.na(age_years))
    labels <- make_labels_age_group_year(breaks = breaks,
                                         open_left = FALSE,
                                         open_right = open_right,
                                         include_na = include_na)
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}


date_to_age_group_month <- function(date, dob,
                                    age_max = 1200,
                                    open_right = TRUE,
                                    as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- demcheck::err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max",
                                               inf_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                        name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    if (!open_right)
        demcheck::err_exceeds_age_max(age = age_months,
                            age_max = age_max,
                            date = date,
                            dob = dob,
                            unit = "month")    
    breaks <- seq.int(from = 0L,
                      to = age_max)
    include_na <- any(is.na(age_months))
    labels <- make_labels_age_group_month(breaks = breaks,
                                          open_left = FALSE,
                                          open_right = open_right,
                                          include_na = include_na)
    i <- findInterval(x = age_months,
                      vec = breaks)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}


date_to_age_group_quarter <- function(date, dob,
                                      age_max = 1200,
                                      open_right = TRUE,
                                      as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- demcheck::err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max",
                                               inf_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                        name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 4L
    if (!open_right)
        demcheck::err_exceeds_age_max(age = age_quarters,
                            age_max = age_max,
                            date = date,
                            dob = dob,
                            unit = "quarter")    
    breaks <- seq.int(from = 0L,
                      to = age_max)
    include_na <- any(is.na(age_quarters))
    labels <- make_labels_age_group_quarter(breaks = breaks,
                                            open_left = FALSE,
                                            open_right = open_right,
                                            include_na = include_na)
    i <- findInterval(x = age_quarters,
                      vec = breaks)
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans
}


