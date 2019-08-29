
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
    if (all(is.na(date) | is.na(dob)))
        return(rep(NA_character_, times = length(date)))
    uses_breaks <- !is.null(breaks)
    if (uses_breaks) {
        breaks <- err_tdy_breaks(breaks)
    }
    else {
        age_open <- err_tdy_positive_integer_scalar(x = age_open,
                                                    name = "age_open")
        breaks <- seq.int(from = 0L, to = age_open)
    }
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    n <- length(breaks)
    open_left <- any(age_years < breaks[[1L]], na.rm = TRUE)
    open_right <- any(age_years >= breaks[[n]], na.rm = TRUE)
    labels <- make_labels_age_group(breaks = breaks,
                                    open_left = open_left,
                                    open_right = open_right)
    i <- findInterval(x = age_years, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}


#' @rdname date_to_age_group
#' @export
date_to_age_group_lifetable <- function(date, dob,
                                        age_max = 100,
                                        as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    if (all(is.na(date) | is.na(dob)))
        return(rep(NA_character_, times = length(date)))
    uses_breaks <- !is.null(breaks)
    if (uses_breaks) {
        breaks <- err_tdy_breaks(breaks)
    }
    else {
        age_open <- err_tdy_positive_integer_scalar(x = age_open,
                                                    name = "age_open")
        breaks <- seq.int(from = 0L, to = age_open)
    }
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    n <- length(breaks)
    open_left <- any(age_years < breaks[[1L]], na.rm = TRUE)
    open_right <- any(age_years >= breaks[[n]], na.rm = TRUE)
    labels <- make_labels_age_group(breaks = breaks,
                                    open_left = open_left,
                                    open_right = open_right)
    i <- findInterval(x = age_years, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}


date_to_age_group_fert <- function(date, dob,
                                   age_min = 15,
                                   age_max = 50,
                                   recode_up = TRUE,
                                   recode_down = TRUE,
                                   as_factor = TRUE) {
}


#' @rdname date_to_age_group
#' @export
date_to_age_group_custom <- function(date, dob,
                               open_right = TRUE,
                               breaks = NULL,
                               as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    if (all(is.na(date) | is.na(dob)))
        return(rep(NA_character_, times = length(date)))
    uses_breaks <- !is.null(breaks)
    if (uses_breaks) {
        breaks <- err_tdy_breaks(breaks)
    }
    else {
        age_open <- err_tdy_positive_integer_scalar(x = age_open,
                                                    name = "age_open")
        breaks <- seq.int(from = 0L, to = age_open)
    }
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    n <- length(breaks)
    open_left <- any(age_years < breaks[[1L]], na.rm = TRUE)
    open_right <- any(age_years >= breaks[[n]], na.rm = TRUE)
    labels <- make_labels_age_group(breaks = breaks,
                                    open_left = open_left,
                                    open_right = open_right)
    i <- findInterval(x = age_years, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}


#' @rdname date_to_age_group
#' @export
date_to_age_group_month <- function(date, dob,
                                    age_max = 1200,
                                    open_right = TRUE,
                                    as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    is_open_right <- !is.null(age_open)
    if (is_open_right)
        age_open <- err_tdy_positive_integer_scalar(x = age_open,
                                                    name = "age_open")
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age <- age_completed_months(date = date,
                                dob = dob)
    open_right <- any(age >= breaks[[n]], na.rm = TRUE)
    labels <- make_labels_age_group_month(min_break = min_break,
                                          max_break = max_break,
                                          open_left = FALSE,
                                          open_right = open_right)
    i <- findInterval(x = age, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}

#' @rdname date_to_age_group
#' @export
date_to_age_group_quarter <- function(date, dob,
                                      min_break = 0,
                                      max_break = 400,
                                      as_factor = TRUE) {
    l <- err_tdy_date_dob(date = date,
                          dob = dob)
    date <- l$date
    dob <- l$dob
    l <- err_tdy_min_max_break(min_break = min_break,
                               max_break = max_break)
    l$min_break <- min_break
    l$max_break <- max_break
    err_is_logical_flag(x = as_factor,
                        name = "as_factor")
    age_months <- date_to_age_completed_months(date = date,
                                               dob = dob)
    age_quarters <- age_months %/% 4L
    n <- length(breaks)
    if (n > 0L) {
        open_left <- any(age_quarters < breaks[[1L]], na.rm = TRUE)
        open_right <- any(age_quarters >= breaks[[n]], na.rm = TRUE)
    }
    else {
        open_left <- FALSE
        open_right <- FALSE
    }
    labels <- make_labels_age_group_quarter(min_break = min_break,
                                            max_break = max_break,
                                            open_left = open_left,
                                            open_right = open_right)
    i <- findInterval(x = age_quarters, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}



## 'date' and 'dob' POSIXlt vectors with equal
## length and no NAs - typically constructed
## using 'err_tdy_date_dob'
date_to_age_completed_months <- function(date, dob) {
    y2 <- date$year + 1900L
    y1 <- dob$year + 1900L
    m2 <- date$mon + 1L
    m1 <- dob$mon + 1L
    d2 <- date$mday
    d1 <- dob$mday
    is_29feb_2 <- (m2 == 2L) & (d2 == 29L)
    is_29feb_1 <- (m1 == 2L) & (d1 == 29L)
    d2[is_29_feb_2] <- 28L
    d1[is_29_feb_1] <- 28L
    12L * (y2 - y1) + (m2 - m1) - (d2 < d1)
}
