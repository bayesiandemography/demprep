
## HAS_TESTS
#' Convert dates to one-year age groups
#'
#' Given dates when events occurred or measurements were made,
#' and dates of birth, derive age groups. These
#' age groups all have widths of one year, except possibly
#' the final age group.
#'
#' A person belongs to age group \code{"a"} if that
#' person was exactly \code{a} years
#' old at their most recent birthday. For instance, a person
#' who had their fifth birthday two days ago belongs to age
#' group \code{"5"} and a person who was born (had their zero-th
#' birthday) three months ago belongs to age group \code{"0"}.
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
#' When \code{break_max} is \code{NULL}, the oldest age
#' group depends on the highest value in the data.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate age groups,
#' including age groups that not appear in the data.
#'
#' @param date Dates of events or measurements.
#' @param dob Dates of birth.
#' @param break_max An integer, defining the
#' maximum age group.
#' @param open_right Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#' @param as_factor Whether the return value should be a factor.
#' Defaults to \code{TRUE}.
#'
#' @return If \code{as_factor} is \code{TRUE}, a factor;
#' otherwise a character vector. The length equals the length
#' of \code{date} or the length of \code{dob}, whichever
#' is greater.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{date_to_age_group_multi}},
#' \code{\link{date_to_age_group_lifetab}},
#' \code{\link{date_to_age_group_fert}},
#' \code{\link{date_to_age_group_custom}},
#' \code{\link{date_to_age_group_quarter}},
#' and \code{\link{date_to_age_group_month}}.
#' Other functions for working with one-year intervals are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#'
#' @examples
#' date_to_age_group_year(date = c("2024-03-27", "2022-11-09"),
#'                        dob = c("2001-03-21", "2000-07-13"))
#'
#' ## replicate date of birth
#' date_to_age_group_year(date = c("2024-03-27", "2022-11-09"),
#'                        dob = "2011-05-18")
#'
#' ## return non-factor
#' date_to_age_group_year(date = c("2024-03-27", "2022-11-09"),
#'                        dob = "2011-05-18",
#'                        as_factor = FALSE)
#'
#' ## alternative specifications for oldest age group
#' date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01")
#' date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = 80)
#' date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = NULL)
#' date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = NULL,
#'                        open_right = FALSE)
#' @export 
date_to_age_group_year <- function(date,
                                   dob,
                                   break_max = 100,
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
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    if (!is.null(break_max) && !open_right)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    breaks <- make_breaks_integer_year(age = age_years,
                                       width = 1L,
                                       break_max = break_max,
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
#' Convert dates to multi-year age groups
#'
#' Given dates when events occurred or measurements were made,
#' and dates of birth, derive age groups. These
#' age groups all have the same width, except possibly
#' the final age group. The width must be a multiple
#' of one year.
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
#' When \code{break_max} is \code{NULL}, the oldest age
#' group depends on the highest value in the data.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate age groups,
#' including age groups that not appear in the data.
#'
#' @includeParams date_to_age_group_year
#' @param width The width in years of the age intervals.
#' A positive integer. Defaults to 5.
#'
#' @return If \code{as_factor} is \code{TRUE}, a factor;
#' otherwise a character vector. The length equals the length
#' of \code{date} or the length of \code{dob}, whichever
#' is greater.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_age_group_lifetab}},
#' \code{\link{date_to_age_group_fert}},
#' \code{\link{date_to_age_group_custom}},
#' \code{\link{date_to_age_group_quarter}},
#' and \code{\link{date_to_age_group_month}}.
#' Other functions for working with multi-year intervals are
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_cohort_multi}},
#' and \code{\link{date_to_triangle_multi}}.
#'
#' @examples
#' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#'                         dob = c("2001-03-21", "2000-07-13"))
#'
#' ## alternative values for 'width'
#' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#'                         dob = c("2001-03-21", "2000-07-13"),
#'                         width = 10)
#' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#'                         dob = c("2001-03-21", "2000-07-13"),
#'                         width = 1)
#'
#' ## replicate date of birth
#' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#'                         dob = "2011-05-18")
#'
#' ## return non-factor
#' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#'                         dob = "2011-05-18",
#'                         as_factor = FALSE)
#'
#' ## alternative specifications for oldest age group
#' date_to_age_group_multi(date = "2019-09-22",
#'                         dob = "1910-01-01")
#' date_to_age_group_multi(date = "2019-09-22",
#'                         dob = "1910-01-01",
#'                         break_max = 80)
#' date_to_age_group_multi(date = "2019-09-22",
#'                         dob = "1910-01-01",
#'                         break_max = NULL)
#' date_to_age_group_multi(date = "2019-09-22",
#'                         dob = "1910-01-01",
#'                         break_max = NULL,
#'                         open_right = FALSE)
#' @export
date_to_age_group_multi <- function(date,
                                    dob,
                                    width = 5,
                                    break_max = 100,
                                    open_right = TRUE,
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
    if (!is.null(break_max))
        demcheck::err_is_logical_flag(x = open_right,
                                      name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    demcheck::err_is_multiple_of(x1 = break_max,
                                 x2 = width,
                                 name1 = "break_max",
                                 name2 = "width",
                                 null_ok = TRUE)
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    if (!open_right)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")    
    breaks <- make_breaks_integer_year(age = age_years,
                                       width = width,
                                       break_max = break_max,
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
#' Convert dates to age groups used in abridged life table
#'
#' Given dates of death and dates of birth,
#' derive age groups. These
#' age groups are the ones typically used in an
#' "abridged" (ie not single-year) life table: "0", "1-4",
#' and "5-9", "10-14", "10-14", and so on up to the
#' highest age group.
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_max} is used to specify
#' the oldest age group, which is always open
# on the right.
#'
#' When \code{as_factor} is \code{TRUE} the levels of
#' the factor include all intermediate age groups,
#' including age groups that not appear in the data.
#'
#' @includeParams date_to_age_group_year
#' @param date Date of death.
#'
#' @return If \code{as_factor} is \code{TRUE}, a factor;
#' otherwise a character vector. The length equals the length
#' of \code{date} or the length of \code{dob}, whichever
#' is greater.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_age_group_multi}},
#' \code{\link{date_to_age_group_fert}},
#' \code{\link{date_to_age_group_custom}},
#' \code{\link{date_to_age_group_quarter}},
#' and \code{\link{date_to_age_group_month}}.
#'
#' @examples
#' date_to_age_group_lifetab(date = c("2024-03-27", "2022-11-09"),
#'                           dob = c("2001-03-21", "2000-07-13"))
#'
#' ## replicate date of birth
#' date_to_age_group_lifetab(date = c("2024-03-27", "2022-11-09"),
#'                           dob = "2011-05-18")
#'
#' ## return non-factor
#' date_to_age_group_lifetab(date = c("2024-03-27", "2022-11-09"),
#'                           dob = "2011-05-18",
#'                           as_factor = FALSE)
#'
#' ## alternative specifications for oldest age group
#' date_to_age_group_multi(date = "2019-09-22",
#'                         dob = "1910-01-01")
#' date_to_age_group_multi(date = "2019-09-22",
#'                         dob = "1910-01-01",
#'                         break_max = 80)
#' date_to_age_group_multi(date = "2019-09-22",
#'                         dob = "1910-01-01",
#'                         break_max = NULL)
#' @export
date_to_age_group_lifetab <- function(date, dob,
                                      break_max = 100,
                                      as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = FALSE)
    demcheck::err_is_multiple_of_n(x = break_max,
                                   name = "break_max",
                                   n = 5L,
                                   null_ok = FALSE)
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    breaks <- make_breaks_integer_lifetab(break_max)
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
#' Convert dates to age groups when measuring fertility 
#'
#' Given the dates when the births occur,
#' and the date of birth of the mothers,
#' derive age groups. These age groups all have the same width,
#' which must be a multiple of one year.
#'
#' \code{date} and \code{dob} are both vectors of class
#' \code{\link[base]{Date}}, or vectors that can be coerced to class
#' \code{Date} via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_min} and \code{break_max} specify
#' the range of ages over which reproduction
#' is assumed to occur. For instance, if
#' \code{break_min} is \code{15} and \code{break_max}
#' is \code{50}, all births are assumed to
#' occur to women aged 15 to 49 (inclusive).
#'
#' Datasets sometimes contain a few births to mothers
#' younger than the minimum, or older than the maximum,
#' expected ages. Demographers often recode births to
#' unexpectedly young mothers as occurring to women of the youngest
#' expected age group, and births to unexpectedly old mothers as occurring
#' to women of the oldest expected age group. This avoids having cells
#' with very small counts, but also allows for the possibility that
#' the extreme reported ages may be coding errors. Recoding of
#' births outside the expected range is controlled by
#' parameters \code{recode_up} and \code{recode_down}. The default
#' is for no recoding to occur.
#'
#' @includeParams date_to_age_group_year
#' @param width The width in years of the age intervals.
#' A positive integer. Defaults to 5.
#' @param recode_up If \code{TRUE}, births to women
#' aged less than is \code{TRUE}, are treated as occurring to
#' women in the lowest repoductive age group.
#' @param recode_down If \code{TRUE}, births to women
#' aged \code{break_max} or higher are treated as
#' occurring to women in the highest reproductive
#' age group.
#'
#' @return If \code{as_factor} is \code{TRUE}, a factor;
#' otherwise a character vector. The length equals the length
#' of \code{date} or the length of \code{dob}, whichever
#' is greater.
#'
#' @seealso Other functions for creating age groups are
#' \code{\link{date_to_age_group_year}},
#' \code{\link{date_to_age_group_multi}},
#' \code{\link{date_to_age_group_lifetab}},
#' \code{\link{date_to_age_group_custom}},
#' \code{\link{date_to_age_group_quarter}},
#' and \code{\link{date_to_age_group_month}}.
#'
#' @examples
#' date_to_age_group_fert(date = c("2024-03-27", "2022-11-09"),
#'                        dob = c("2001-03-21", "2000-07-13"))
#'
#' ## alternative values for 'width'
#' date_to_age_group_fert(date = c("2024-03-27", "2022-11-09"),
#'                        dob = c("2001-03-21", "2000-07-13"),
#'                        width = 10,
#'                        break_min = 20)
#' date_to_age_group_fert(date = c("2024-03-27", "2022-11-09"),
#'                        dob = c("2001-03-21", "2000-07-13"),
#'                        width = 1)
#'
#' ## replicate date of birth
#' date_to_age_group_fert(date = c("2024-03-27", "2022-11-09"),
#'                        dob = "2001-05-18")
#'
#' ## return non-factor
#' date_to_age_group_fert(date = c("2024-03-27", "2022-11-09"),
#'                        dob = "2001-05-18",
#'                        as_factor = FALSE)
#'
#' ## allow youngest and oldest age groups to be
#' ## set by the data
#' date_to_age_group_fert(date = c("2052-01-02", "2019-09-22", "2022-10-08"),
#'                        dob = c("2000-01-01", "2001-03-17", "2010-07-05"),
#'                        break_min = NULL,
#'                        break_max = NULL)
#'
#' ## recode ages outside the expected range
#' date_to_age_group_fert(date = c("2052-01-02", "2019-09-22", "2022-10-08"),
#'                        dob = c("2000-01-01", "2001-03-17", "2010-07-05"),
#'                        recode_up = TRUE,
#'                        recode_down = TRUE)
#' @export
date_to_age_group_fert <- function(date, dob,
                                   break_min = 15,
                                   break_max = 50,
                                   width = 5,
                                   recode_up = FALSE,
                                   recode_down = FALSE,
                                   as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
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
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
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
    is_lt_min <- age_years < break_min
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        if (recode_up)
            age_years[is_lt_min] <- break_min
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
        if (recode_down)
            age_years[is_ge_max] <- break_max - 1L
        else {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'break_max' is %d and 'recode_down' is FALSE"),
                          date[[i_ge_max]],
                          dob[[i_ge_max]],
                          age_years[[i_ge_max]],
                          break_max))
        }
    }
    breaks <- seq(from = break_min,
                  to = break_max,
                  by = width)
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

## HAS_TESTS
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
    breaks <- demcheck::err_tdy_breaks_integer(x = breaks,
                                               name = "breaks",
                                               open_left = FALSE,
                                               open_right = open_right)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    is_lt_min <- age_years < breaks[[1L]]
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                            "but minimum value for '%s' is %d"),
                      date[[i_lt_min]],
                      dob[[i_lt_min]],
                      age_years[[i_lt_min]],
                      "breaks",
                      breaks[[1L]]))
    }
    if (!open_right) {
        n <- length(breaks)
        is_ge_max <- age_years >= breaks[[n]]
        i_ge_max <- match(TRUE, is_ge_max, nomatch = 0L)
        if (i_ge_max > 0L) {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'open_right' is FALSE and maximum value for 'breaks' is %d"),
                          date[[i_ge_max]],
                          dob[[i_ge_max]],
                          age_years[[i_ge_max]],
                          breaks[[n]]))
        }
    }
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
date_to_age_group_quarter <- function(date,
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
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    if (!open_right)
        demcheck::err_lt_break_max_age(age = age_quarters,
                                 break_max = break_max,
                                 date = date,
                                 dob = dob,
                                 unit = "quarter")    
    breaks <- seq.int(from = 0L,
                      to = break_max)
    include_na <- any(is.na(age_quarters))
    labels <- make_labels_age_group_quarter(break_min = 0L,
                                            break_max = break_max,
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


#' @rdname date_to_age_group
#' @export
date_to_age_group_month <- function(date, dob,
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
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    if (!open_right)
        demcheck::err_lt_break_max_age(age = age_months,
                                 break_max = break_max,
                                 date = date,
                                 dob = dob,
                                 unit = "month")    
    breaks <- seq.int(from = 0L,
                      to = break_max)
    include_na <- any(is.na(age_months))
    labels <- make_labels_age_group_month(break_min = 0L,
                                          break_max = break_max,
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



