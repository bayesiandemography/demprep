
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
#'
#' If as_factor is TRUE, and date or dob have NAs, then the
#' levels of the answer will include NA.
#' 
#' @examples
#' @rdname date_to_age_group
NULL

## Assume every (non-NA) record belongs to appropriate age range.
## Do not use these functions for filtering.
## Leave NAs in because these records might later have age imputed.



#' @rdname date_to_age_group
#' @export
date_to_age_group_year <- function(date, dob,
                                   age_max = 100,
                                   open_right = TRUE,
                                   as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max")
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    if (!open_right)
        err_exceeds_age_max(age = age_years,
                            age_max = age_max,
                            date = date,
                            dob = dob,
                            unit = "year")    
    breaks <- seq.int(from = 0L,
                      to = age_max)
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

err_exceeds_age_max <- function(age, age_max, date, dob, unit) {
    exceeds_max <- age >= age_max
    if (any(exceeds_max)) {
        i <- match(TRUE, exceeds_max)
        stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d %ss,",
                            "but 'age_max' is %d %ss and 'open_right' is FALSE"),
                      date[[i]],
                      dob[[i]],
                      age[[i]],
                      unit,
                      age_max,
                      unit))
    }
    TRUE
}
    


#' @rdname date_to_age_group
#' @export
date_to_age_group_multi <- function(date, dob,
                                    width = 5,
                                    age_max = 100,
                                    open_right = TRUE,
                                   as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    width <- err_tdy_positive_integer_scalar(x = width,
                                             name = "width")
    age_max <- err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max")
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    err_is_multiple_of(x1 = age_max,
                       x2 = width,
                       name1 = "age_max",
                       name2 = "width")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    if (!open_right)
        err_exceeds_age_max(age = age_years,
                            age_max = age_max,
                            date = date,
                            dob = dob,
                            unit = "year")    
    breaks <- seq.int(from = 0L,
                      to = age_max,
                      by = width)
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



#' @rdname date_to_age_group
#' @export
date_to_age_group_lifetab <- function(date, dob,
                                      age_max = 100,
                                      as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max")
    if (age_max %% 5L != 0L)
        stop(gettextf("'%s' is not divisible by %d",
                      "age_max", 5L))
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    s <- seq.int(from = 5L,
                 to = age_max,
                 by = 5L)
    breaks <- c(0L, 1L, s)
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

date_to_age_group_fert <- function(date, dob,
                                   age_min = 15,
                                   age_max = 50,
                                   width = 1,
                                   recode_up = TRUE,
                                   recode_down = TRUE,
                                   as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_min <- err_tdy_positive_integer_scalar(x = age_min,
                                               name = "age_min")
    age_max <- err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max")
    if (age_min >= age_max)
        stop(gettextf("'%s' [%d] is greater than or equal to '%s' [%d]",
                      "age_min", age_min, "age_max", age_max))
    width <- err_tdy_positive_integer_scalar(x = width,
                                             name = "width")
    if ((age_max - age_min) %% width != 0L)
        stop(gettextf("difference between '%s' [%d] and '%s' [%d] not divisible by '%s' [%d]",
                      "age_max", age_max, "age_min", age_min, "width", width))
    err_is_logical_flag(x = recode_up,
                        name = "recode_up")
    err_is_logical_flag(x = recode_down,
                        name = "recode_down")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    breaks <- seq(from = age_min,
                  to = age_max,
                  by = width)
    is_lt_min <- age_years < age_min
    if (any(is_lt_min)) {
        if (recode_up)
            age_years[is_lt_min] <- age_min
        else {
            i <- match(TRUE, is_lt_min)
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'age_min' is %d and 'recode_up' is FALSE",
                                date[[i]],
                                dob[[i]],
                                age[[i]],
                                age_min))
            }
        }
    }
    is_ge_max <- age_years >= age_max
    if (any(is_ge_max)) {
        if (recode_down)
            age_years[is_ge_max] <- age_max - 1L
        else {
            i <- match(TRUE, is_ge_max)
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'age_max' is %d and 'recode_down' is FALSE"),
                          date[[i]],
                          dob[[i]],
                          age[[i]],
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
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    breaks <- err_tdy_breaks_age(x = breaks,
                                 name = "breaks")
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    is_lt_min <- age_years < breaks[[1L]]
    if (any(is_lt_min)) {
        i <- match(TRUE, is_lt_min)
        stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                            "but minimum value for '%s' is %d")
                      date[[i]],
                      dob[[i]],
                      age[[i]],
                      "breaks",
                      breaks[[1L]]))
    }
    if (!open_right)
        err_exceeds_age_max(age = age_years,
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
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max")
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    if (!open_right)
        err_exceeds_age_max(age = age_months,
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
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    age_max <- err_tdy_positive_integer_scalar(x = age_max,
                                               name = "age_max")
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 4L
    if (!open_right)
        err_exceeds_age_max(age = age_quarters,
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


