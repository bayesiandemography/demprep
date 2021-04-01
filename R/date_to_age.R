

## Note - all functions return factor with NAs but with
## meaningful levels when supplied with empty inputs,
## except in functions having 'break_min' where value
## for 'break_min' not supplied (and hence there is not
## enough information to form levels).

## HAS_TESTS
#' Convert dates to one-year age groups
#'
#' Given the dates when events occurred,
#' and the dates of birth of the people experiencing the events,
#' allocate the events to age groups.
#' All the resulting age groups have widths
#' of one year, except possibly the final age group.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} years
#' old at their most recent birthday. For instance, a person
#' belongs to age group {"5"} if that person had their
#' 5th birthday two days ago, and a person belongs to age
#' group \code{"0"} if that person had their 0th birthday
#' (ie was born) three months ago.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' argument with length 1 is recycled.
#'
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{date_to_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' When \code{open_last} is \code{TRUE}, the final age group
#' is "open", meaining that it has no upper limit.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate age groups,
#' including ones that do not appear in the data.
#' 
#' If \code{date} or \code{dob} contain \code{NA}, then the
#' levels of the factor created by \code{date_to_age_year}
#' will also contain \code{NA}.
#'
#' @param date Dates of events.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class
#' \code{Date} using function \code{\link[base]{as.Date}}.
#' @param dob Dates of birth.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class
#' \code{Date} using function \code{\link[base]{as.Date}}.
#' @param break_min An integer or \code{NULL}.
#' Defaults to 0.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 100.
#' @param open_last Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#'
#' @return A factor with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_multi}},
#' \code{\link{date_to_age_lifetab}},
#' \code{\link{date_to_age_births}},
#' \code{\link{date_to_age_custom}},
#' \code{\link{date_to_age_quarter}},
#' and \code{\link{date_to_age_month}}.
#'
#' Other functions for creating one-year
#' intervals from dates are
#' \code{\link{date_to_period_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#'
#' Existing labels for one-year age groups
#' can be processed using \code{\link{clean_age}}
#' and \code{\link{format_age_year}}.
#' 
#' @examples
#' date_to_age_year(date = c("2024-03-27",
#'                           "2022-11-09"),
#'                  dob = c("2001-03-21",
#'                          "2000-07-13"))
#'
#' ## replicate date of birth
#' date_to_age_year(date = c("2024-03-27",
#'                           "2022-11-09"),
#'                  dob = "2011-05-18")
#'
#' ## specify alternative values for the
#' ## lowest and highest breaks
#' date_to_age_year(date = c("2024-03-27",
#'                           "2022-11-09"),
#'                  dob = c("2001-03-21",
#'                          "2000-07-13"),
#'                  break_min = 20,
#'                  break_max = 30)
#' 
#' ## allow lowest age group to be set by the data
#' date_to_age_year(date = c("2024-03-27",
#'                           "2022-11-09"),
#'                  dob = c("2001-03-21",
#'                          "2000-07-13"),
#'                  break_min = NULL,
#'                  break_max = 30)
#'
#' ## highest age group is closed rather
#' ## than open
#' date_to_age_year(date = c("2024-03-27",
#'                           "2022-11-09"),
#'                  dob = c("2001-03-21",
#'                          "2000-07-13"),
#'                  break_min = NULL,
#'                  break_max = 30,
#'                  open_last = FALSE)
#' @export 
date_to_age_year <- function(date,
                             dob,
                             break_min = 0,
                             break_max = 100,
                             open_last = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                               name = "break_min",
                                                               null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with "undefined" case where there are no valid date-dob pairs
    ## and where 'break_min' or 'break_max' is missing,
    ## so cannot construct levels
    n_date <- length(date)
    is_empty <- all(is.na(date) | is.na(dob))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (is_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n_date)
        ans <- factor(ans,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## get age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## check that all ages greater than or equal to 'break_min'
    if (!is.null(break_min)) {
        demcheck::err_ge_break_min_age(age = age_years,
                                       break_min = break_min,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    }
    ## if final interval not open, check that all
    ## ages less than 'break_max'
    if (!open_last && !is.null(break_max))
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    ## make breaks
    breaks <- make_breaks_date_to_integer_year(age = age_years,
                                               width = 1L,
                                               break_min = break_min,
                                               break_max = break_max,
                                               has_break_min_arg = TRUE,
                                               has_break_max_arg = TRUE)
    ## make labels for these breaks
    include_na <- anyNA(date) || anyNA(dob)
    labels <- make_labels_age(breaks = breaks,
                              open_last = open_last,
                              include_na = include_na)
    ## assign labels to ages
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}    

## HAS_TESTS
#' Convert dates to multi-year age groups
#'
#' Given the dates when events occurred,
#' and the dates of birth of the people experiencing the events,
#' allocate the events to age groups. These
#' age groups all have the same width, except possibly
#' the final age group. The width is measured in years
#' and must be an integer.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{date_to_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' When \code{open_last} is \code{TRUE}, the final age group
#' is "open", meaning that it has no upper limit.
#' 
#' The return value is a factor. The levels of this
#' factor contain all intermediate age groups,
#' including ones that do not appear in the data.
#'
#' If \code{date} or \code{dob} contain \code{NA}, then the
#' levels of the factor created by \code{date_to_age_multi}
#' will also contain \code{NA}.
#'
#' @inheritParams date_to_age_year
#' @param width The width in years of the age intervals.
#' A positive integer. Defaults to 5.
#'
#' @return A factor with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_age_lifetab}},
#' \code{\link{date_to_age_births}},
#' \code{\link{date_to_age_custom}},
#' \code{\link{date_to_age_quarter}},
#' and \code{\link{date_to_age_month}}.
#'
#' Other functions for creating multi-year
#' intervals from dates are
#' \code{\link{date_to_period_multi}},
#' \code{\link{date_to_cohort_multi}},
#' and \code{\link{date_to_triangle_multi}}.
#'
#' Existing labels for multi-year age groups
#' can be processed using \code{\link{clean_age}}
#' and \code{\link{format_age_multi}}.
#' 
#' @examples
#' date_to_age_multi(date = c("2024-03-27",
#'                            "2022-11-09"),
#'                   dob = c("2001-03-21",
#'                           "2000-07-13"))
#'
#' ## alternative value for 'width'
#' date_to_age_multi(date = c("2024-03-27",
#'                            "2022-11-09"),
#'                   dob = c("2001-03-21",
#'                           "2000-07-13"),
#'                   width = 10)
#'
#' ## final age group closed
#' date_to_age_multi(date = c("2024-03-27",
#'                            "2022-11-09"),
#'                   dob = c("2001-03-21",
#'                           "2000-07-13"),
#'                   width = 10,
#'                   open_last = FALSE)
#'
#' ## allow highest age group to be set by the data
#' date_to_age_multi(date = c("2024-03-27",
#'                            "2022-11-09"),
#'                   dob = c("2001-03-21",
#'                           "2000-07-13"),
#'                   width = 10,
#'                   break_max = NULL)
#' @export
date_to_age_multi <- function(date,
                              dob,
                              width = 5,
                              break_min = 0,
                              break_max = 100,
                              open_last = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                               name = "break_min",
                                                               null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_gt_scalar(x1 = break_max,
                                x2 = break_min,
                                name1 = "break_max",
                                name2 = "break_min")
        demcheck::err_difference_divisible(x1 = break_max,
                                           x2 = break_min,
                                           y = width,
                                           name1 = "break_max",
                                           name2 = "break_min",
                                           name_y = "width")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with "undefined" case where there are no valid date-dob pairs
    ## and where 'break_min' or 'break_max' is missing,
    ## so cannot construct levels
    n_date <- length(date)
    is_empty <- all(is.na(date) | is.na(dob))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (is_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n_date)
        ans <- factor(ans,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## get age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## check that all ages greater than 'break_min'
    if (!is.null(break_min)) {
        demcheck::err_ge_break_min_age(age = age_years,
                                       break_min = break_min,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    }
    ## if final interval not open, check that all
    ## ages less than 'break_max'
    if (!open_last)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")    
    ## make breaks
    breaks <- make_breaks_date_to_integer_year(age = age_years,
                                               width = width,
                                               break_min = break_min,
                                               break_max = break_max,
                                               has_break_min_arg = TRUE,
                                               has_break_max_arg = TRUE)
    ## make labels for these breaks
    include_na <- anyNA(date) || anyNA(dob)
    labels <- make_labels_age(breaks = breaks,
                              open_last = open_last,
                              include_na = include_na)
    ## assign labels to ages
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to age groups used in abridged life table
#'
#' Given dates of death and dates of birth,
#' allocate the deaths to age groups. These
#' age groups are the ones typically used in
#' "abridged" (ie not single-year) life tables: "0", "1-4",
#' and "5-9", "10-14", and so on up to the
#' highest age group, which is always open.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_max} is used to specify
#' the oldest age group.
#' If \code{break_max} is \code{NULL}, the oldest
#' age group is derived from the data.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate age groups,
#' including ones that do not appear in the data.
#'
#' If \code{date} or \code{dob} contain \code{NA}, then the
#' levels of the factor created by \code{date_to_age_lifetab}
#' will also contain \code{NA}.
#'
#' @inheritParams date_to_age_year
#' @param date Date of death.
#'
#' @return A factor with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_age_multi}},
#' \code{\link{date_to_age_births}},
#' \code{\link{date_to_age_custom}},
#' \code{\link{date_to_age_quarter}},
#' and \code{\link{date_to_age_month}}.
#'
#' Existing labels for life table age groups
#' can be processed using \code{\link{clean_age}}
#' and \code{\link{format_age_lifetab}}.
#' 
#' @examples
#' date_to_age_lifetab(date = c("2024-03-27",
#'                              "2022-11-09"),
#'                     dob = c("2001-03-21",
#'                             "2000-07-13"))
#'
#' ## set oldest age group to 80+
#' date_to_age_lifetab(date = "2019-09-22",
#'                     dob = "1910-01-01",
#'                     break_max = 80)
#' @export
date_to_age_lifetab <- function(date,
                                dob,
                                break_max = 100) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_max)) {
        demcheck::err_multiple_of_n(x = break_max,
                                    name = "break_max",
                                    n = 5L,
                                    null_ok = FALSE)
    }
    ## deal with "undefined" case where there
    ## are no valid date-dob pairs
    ## and where 'break_max' is missing,
    ## so cannot construct levels
    n_date <- length(date)
    is_empty <- all(is.na(date) | is.na(dob))
    is_unbounded <- is.null(break_max)
    if (is_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n_date)
        ans <- factor(ans,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## get age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## make breaks
    breaks <- make_breaks_date_to_integer_lifetab(age = age_years,
                                                  break_max = break_max)
    ## make labels for breaks
    include_na <- anyNA(date) || anyNA(dob)
    labels <- make_labels_age(breaks = breaks,
                              open_last = TRUE,
                              include_na = include_na)
    ## assign labels to ages
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to age groups when tabulating births 
#'
#' Given the dates when births occur,
#' and the dates of birth of the parents (typically mothers),
#' allocate the births to age groups. These age groups all
#' have the same width, which is measured in whole years.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{break_min} and \code{break_max} specify
#' the range of ages over which reproduction
#' is assumed to occur. If, for instance,
#' \code{break_min} is \code{15} and \code{break_max}
#' is \code{50}, all births are assumed to
#' occur to women aged 15 to 49 (inclusive).
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{date_to_age_births}
#' finds the narrowest range that accommodates the data.
#'
#' Datasets sometimes contain a few births to parents
#' younger than the assumed minimum age of reproduction,
#' or births to parents older than the assumed maximum age
#' of reproduction. Demographers often recode such births,
#' so that ones to unexpectedly young parents are
#' treated as occurring just above the minimum age
#' for reproduction, and ones to unexpectedly old parents
#' are treated as occurring just below the maximum
#' age for reproduction. This recoding can be justified
#' on the grounds that some of the original ages may have
#' been misreported, but it also alleviates any problems
#' with tabulations having small counts at extreme ages.
#' Recoding of parents' ages outside the expected range
#' is controlled by parameters \code{recode_up}
#' and \code{recode_down}. The default
#' is for no recoding to occur.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate age groups,
#' including ones that do not appear in the data.
#'
#' If \code{date} or \code{dob} contain \code{NA}, then the
#' levels of the factor created by \code{date_to_age_births}
#' will also contain \code{NA}.
#'
#' @param date Dates when births being tabulated occur.
#' @param dob Dates of birth of parents.
#' @param break_min An integer or \code{NULL}.
#' Defaults to 15.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 50.
#' @param width The width in years of the age intervals.
#' A positive integer defaulting to 5.
#' @param recode_up If \code{TRUE}, births to parents
#' aged less than \code{break_min} are treated as occurring to
#' people in the lowest repoductive age group.
#' @param recode_down If \code{TRUE}, births to parents
#' aged \code{break_max} or more are treated as
#' occurring to people in the highest reproductive
#' age group.
#'
#' @return A factor with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_age_multi}},
#' \code{\link{date_to_age_lifetab}},
#' \code{\link{date_to_age_custom}},
#' \code{\link{date_to_age_quarter}},
#' and \code{\link{date_to_age_month}}.
#'
#' Existing labels for age groups from births
#' can be processed using \code{\link{clean_age}}
#' and \code{\link{format_age_births}}.
#' 
#' @examples
#' date_to_age_births(date = c("2024-03-27",
#'                             "2022-11-09"),
#'                    dob = c("2001-03-21",
#'                            "2000-07-13"))
#'
#' date_to_age_births(date = c("2024-03-27",
#'                             "2022-11-09"),
#'                    dob = c("2001-03-21",
#'                            "2000-07-13"),
#'                    width = 10,
#'                    break_min = 20)
#'
#' date_to_age_births(date = c("2024-03-27",
#'                             "2022-11-09"),
#'                    dob = c("2001-03-21",
#'                            "2000-07-13"),
#'                    width = 1)
#'
#' ## replicate date of birth
#' date_to_age_births(date = c("2024-03-27",
#'                             "2022-11-09"),
#'                    dob = "2001-05-18")
#'
#' ## allow youngest and oldest age groups to be
#' ## set by the data
#' date_to_age_births(date = c("2052-01-02",
#'                             "2019-09-22",
#'                             "2022-10-08"),
#'                    dob = c("2000-01-01",
#'                            "2001-03-17",
#'                            "2010-07-05"),
#'                    break_min = NULL,
#'                    break_max = NULL)
#'
#' ## recode ages outside the expected range
#' date_to_age_births(date = c("2052-01-02",
#'                             "2019-09-22",
#'                             "2022-10-08"),
#'                    dob = c("2000-01-01",
#'                            "2001-03-17",
#'                            "2010-07-05"),
#'                    recode_up = TRUE,
#'                    recode_down = TRUE)
#' @export
date_to_age_births <- function(date, dob,
                               break_min = 15,
                               break_max = 50,
                               width = 5,
                               recode_up = FALSE,
                               recode_down = FALSE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_min <- demcheck::err_tdy_positive_integer_scalar(x = break_min,
                                                           name = "break_min",
                                                           null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_gt_scalar(x1 = break_max,
                                x2 = break_min,
                                name1 = "break_max",
                                name2 = "break_min")
        demcheck::err_difference_divisible(x1 = break_max,
                                           x2 = break_min,
                                           y = width,
                                           name1 = "break_max",
                                           name2 = "break_min",
                                           name_y = "width")
    }
    demcheck::err_is_logical_flag(x = recode_up,
                                  name = "recode_up")
    demcheck::err_is_logical_flag(x = recode_down,
                                  name = "recode_down")
    ## deal with "undefined" case where there are no valid date-dob pairs
    ## and where 'break_min' or 'break_max' is missing,
    ## so cannot construct levels
    n_date <- length(date)
    is_empty <- all(is.na(date) | is.na(dob))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (is_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n_date)
        ans <- factor(ans,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## get age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## check that ages lie within limits implied by 'break_min' and 'break_max'
    if (!is.null(break_min)) {
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
                              break_min),
                     call. = FALSE)
            }
        }
    }
    if (!is.null(break_max)) {
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
                              break_max),
                     call. = FALSE)
            }
        }
    }
    ## make breaks
    breaks <- make_breaks_date_to_integer_births(age = age_years,
                                         width = width,
                                         break_min = break_min,
                                         break_max = break_max)
    ## make labels for breaks
    include_na <- anyNA(date) || anyNA(dob)
    labels <- make_labels_age(breaks = breaks,
                              open_last = FALSE,
                              include_na = include_na)
    ## assign labels to ages
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to customised age groups
#'
#' Given the dates when events occurred,
#' and the dates of birth of the people experiencing the events,
#' allocate the events to age groups.
#' \code{date_to_age_custom} is the most flexible
#' of the \code{date_to_age} functions
#' in that the age groups can have any combination of widths,
#' though the widths must be defined in whole numbers of years.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' \code{breaks} is used to specify the points at which
#' each age group starts and finishes. If 
#' \code{open_last} is \code{TRUE}, and \code{b} is
#' the last value for \code{breaks}, then the oldest
#' age group is \code{[b, Inf)} years. 
#' If \code{open_last} is \code{FALSE}, \code{a} is the
#' second-to-last value for \code{breaks} and \code{b}
#' is the last value, then the oldest age
#' group is \code{[a, b)} years.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate age groups,
#' including ones that do not appear in the data.
#'
#' If \code{date} or \code{dob} contain \code{NA}, then the
#' levels of the factor created by \code{date_to_age_custom}
#' will also contain \code{NA}.
#' 
#' @inheritParams date_to_age_year
#' @param breaks A vector of strictly increasing integer values.
#'
#' @return A factor with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso Other functions for creating age groups
#' from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_age_multi}},
#' \code{\link{date_to_age_lifetab}},
#' \code{\link{date_to_age_births}},
#' \code{\link{date_to_age_quarter}},
#' and \code{\link{date_to_age_month}}.
#'
#' Existing labels for customised age groups
#' can be processed using \code{\link{clean_age}}
#' and \code{\link{format_age_custom}}.
#' 
#' @examples
#' date_to_age_custom(date = c("2024-03-27",
#'                             "2022-11-09"),
#'                    dob = c("2001-03-21",
#'                            "2000-07-13"),
#'                    breaks = c(0, 15, 60))
#' date_to_age_custom(date = c("2024-03-27",
#'                             "2022-11-09"),
#'                    dob = c("2001-03-21",
#'                            "2000-07-13"),
#'                    breaks = c(15, 40, 65))
#'
#' ## alternative specifications for oldest age group
#' date_to_age_custom(date = c("2024-03-27",
#'                             "2022-11-09"),
#'                    dob = c("2001-03-21",
#'                            "2000-07-13"),
#'                    breaks = c(15, 65, 100))
#' date_to_age_custom(date = c("2024-03-27",
#'                             "2022-11-09"),
#'                    dob = c("2001-03-21",
#'                            "2000-07-13"),
#'                    breaks = c(15, 65, 100),
#'                    open_last = FALSE)
#' @export
date_to_age_custom <- function(date,
                               dob,
                               breaks = NULL,
                               open_last = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    breaks <- demcheck::err_tdy_breaks_integer_age(breaks = breaks,
                                                   open_last = open_last)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with case where 'breaks' has length 0
    n_break <- length(breaks)
    n_date <- length(date)
    if (n_break == 0L) {
        if (n_date == 0L) {
            ans <- factor()
            return(ans)
        }
        else
            stop(gettextf("'%s' has length %d",
                          "breaks", 0L),
                 call. = FALSE)
    }
    ## get age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## check that ages lie within limits implied by 'breaks' and 'open_last'
    is_lt_min <- age_years < breaks[[1L]]
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                            "but minimum value for '%s' is %d"),
                      date[[i_lt_min]],
                      dob[[i_lt_min]],
                      age_years[[i_lt_min]],
                      "breaks",
                      breaks[[1L]]),
             call. = FALSE)
    }
    if (!open_last) {
        is_ge_max <- age_years >= breaks[[n_break]]
        i_ge_max <- match(TRUE, is_ge_max, nomatch = 0L)
        if (i_ge_max > 0L) {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'open_last' is FALSE and maximum value for 'breaks' is %d"),
                          date[[i_ge_max]],
                          dob[[i_ge_max]],
                          age_years[[i_ge_max]],
                          breaks[[n_break]]),
                 call. = FALSE)
        }
    }
    ## make labels for breaks
    include_na <- anyNA(date) || anyNA(dob)
    labels <- make_labels_age(breaks = breaks,
                              open_last = open_last,
                              include_na = include_na)
    ## assign labels to ages
    i <- findInterval(x = age_years,
                      vec = breaks)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to quarter (three-month) age groups
#'
#' Given the dates when events occurred,
#' and the dates of birth of the people experiencing the events,
#' allocate the events to age groups. The resulting
#' age groups all have widths of one quarter (ie three months),
#' except possibly the final age group.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} quarters
#' old at their most recent birthday. For instance, a person
#' belongs to age group \code{"20q"} if that person had
#' their 5th birthday (= 20 quarters) two days ago.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{date_to_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' When \code{open_last} is \code{TRUE}, the final age group
#' is "open", meaning that it has no upper limit.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate age groups,
#' including ones that do not appear in the data.
#'
#' If \code{date} or \code{dob} contain \code{NA}, then the
#' levels of the factor created by \code{date_to_age_quarter}
#' will also contain \code{NA}.
#' 
#' @inheritParams date_to_age_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 400.
#'
#' @return A factor with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_age_multi}},
#' \code{\link{date_to_age_lifetab}},
#' \code{\link{date_to_age_births}},
#' \code{\link{date_to_age_custom}},
#' and \code{\link{date_to_age_month}}.
#'
#' Other functions for creating one-quarter
#' intervals from dates are
#' \code{\link{date_to_period_quarter}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#'
#' Existing labels for quarter age groups
#' can be processed using \code{\link{clean_age}}
#' and \code{\link{format_age_quarter}}.
#' 
#' @examples
#' date_to_age_quarter(date = c("2024-03-27",
#'                              "2022-11-09"),
#'                     dob = c("2001-03-21",
#'                             "2000-07-13"))
#'
#' ## specify highest age group
#' date_to_age_quarter(date = "2019-09-22",
#'                     dob = "2010-01-01",
#'                     break_max = 48)
#'
#' ## let lowest age group be set by the data
#' date_to_age_quarter(date = "2019-09-22",
#'                     dob = "2010-01-01",
#'                     break_min = NULL,
#'                     break_max = 48)
#'
#' ## make final age group closed
#' date_to_age_quarter(date = "2019-09-22",
#'                     dob = "2010-01-01",
#'                     break_min = NULL,
#'                     break_max = 48,
#'                     open_last = FALSE)
#' @export
date_to_age_quarter <- function(date,
                                dob,
                                break_min = 0,
                                break_max = 400,
                                open_last = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                               name = "break_min",
                                                               null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with "undefined" case where there are no valid date-dob pairs
    ## and where 'break_min' or 'break_max' is missing,
    ## so cannot construct levels
    n_date <- length(date)
    is_empty <- all(is.na(date) | is.na(dob))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (is_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n_date)
        ans <- factor(ans,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## get age in months and quarters    
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    ## check that all ages greater than 'break_min'
    if (!is.null(break_min)) {
        demcheck::err_ge_break_min_age(age = age_quarters,
                                       break_min = break_min,
                                       date = date,
                                       dob = dob,
                                       unit = "quarter")
    }
    ## if final interval not open, check that all
    ## ages less than 'break_max'
    if (!is.null(break_max) && !open_last)
        demcheck::err_lt_break_max_age(age = age_quarters,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "quarter")
    ## make breaks
    breaks <- make_breaks_date_to_integer_month_quarter(age = age_quarters,
                                                        break_min = break_min,
                                                        break_max = break_max,
                                                        has_break_min_arg = TRUE,
                                                        has_break_max_arg = TRUE)
    ## make labels for these breaks
    n_break <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n_break]]
    include_na <- anyNA(date) || anyNA(dob)
    labels <- make_labels_age_quarter(break_min = break_min,
                                      break_max = break_max,
                                      open_last = open_last,
                                      include_na = include_na)
    ## assign labels to ages
    i <- findInterval(x = age_quarters,
                      vec = breaks)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}

## HAS_TESTS
#' Convert dates to one-month age groups
#'
#' Given the dates when events occurred,
#' and the dates of birth of the people experiencing the events,
#' allocate the events to age groups. These
#' age groups all have widths of one month,
#' except possibly the final age group.
#'
#' A person belongs to age group \code{"x"} if that
#' person was exactly \code{x} months
#' old at their most recent birthday.
#' For instance, a person belongs to age
#' group \code{"60m"} if that person had their
#' 5th birthday (= 60 months) two days ago.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' If \code{break_min} or \code{break_max} is set to \code{NULL},
#' rather than to a specific value, then \code{date_to_age_year}
#' finds the narrowest range that accommodates the data.
#'
#' When \code{open_last} is \code{TRUE}, the final age group
#' is "open", meaning that it has no upper limit.
#'
#' The return value is a factor. The levels of this
#' factor contain all intermediate age groups,
#' including ones that do not appear in the data.
#'
#' If \code{date} or \code{dob} contain \code{NA}, then the
#' levels of the factor created by \code{date_to_age_custom}
#' will also contain \code{NA}.
#' 
#' @inheritParams date_to_age_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 1200.
#'
#' @return A factor with the same length as
#' \code{date} or \code{dob}, whichever
#' is longer.
#'
#' @seealso Other functions for creating
#' age groups from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_age_multi}},
#' \code{\link{date_to_age_lifetab}},
#' \code{\link{date_to_age_births}},
#' \code{\link{date_to_age_custom}},
#' \code{\link{date_to_age_quarter}}.
#'
#' Other functions for creating one-month
#' intervals from dates are
#' \code{\link{date_to_period_month}},
#' \code{\link{date_to_cohort_month}},
#' and \code{\link{date_to_triangle_month}}.
#'
#' Existing labels for month age groups
#' can be processed using \code{\link{clean_age}}
#' and \code{\link{format_age_month}}.
#'
#' @examples
#' date_to_age_month(date = c("2024-03-27",
#'                            "2022-11-09"),
#'                   dob = c("2021-03-21",
#'                           "2020-07-13"))
#'
#' ## specify highest age group
#' date_to_age_month(date = "2019-10-22",
#'                   dob = "2018-01-01",
#'                   break_max = 24)
#'
#' ## let lowest age group be set by the data
#' date_to_age_month(date = "2019-10-22",
#'                   dob = "2018-01-01",
#'                   break_min = NULL,
#'                   break_max = 24)
#'
#' ## make final age group closed
#' date_to_age_month(date = "2019-10-22",
#'                   dob = "2018-01-01",
#'                   break_min = NULL,
#'                   break_max = 24,
#'                   open_last = FALSE)
#' @export
date_to_age_month <- function(date,
                              dob,
                              break_min = 0,
                              break_max = 1200,
                              open_last = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                               name = "break_min",
                                                               null_ok = TRUE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_min) && !is.null(break_max)) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with "undefined" case where there are no valid date-dob pairs
    ## and where 'break_min' or 'break_max' is missing,
    ## so cannot construct levels
    n_date <- length(date)
    is_empty <- all(is.na(date) | is.na(dob))
    is_unbounded <- is.null(break_min) || is.null(break_max)
    if (is_empty && is_unbounded) {
        ans <- rep(NA_character_, times = n_date)
        ans <- factor(ans,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## get age in months
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    ## check that all ages greater than 'break_min'
    if (!is.null(break_min)) {
        demcheck::err_ge_break_min_age(age = age_months,
                                       break_min = break_min,
                                       date = date,
                                       dob = dob,
                                       unit = "month")
    }
    ## if final interval not open, check that all
    ## ages less than 'break_max'
    if (!open_last && !is.null(break_max))
        demcheck::err_lt_break_max_age(age = age_months,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "month")
    ## make breaks
    breaks <- make_breaks_date_to_integer_month_quarter(age = age_months,
                                                        break_min = break_min,
                                                        break_max = break_max,
                                                        has_break_min_arg = TRUE,
                                                        has_break_max_arg = TRUE)
    ## make labels for these breaks
    n_break <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n_break]]
    include_na <- anyNA(date) || anyNA(dob)
    labels <- make_labels_age_month(break_min = break_min,
                                    break_max = break_max,
                                    open_last = open_last,
                                    include_na = include_na)
    ## assign labels to ages
    i <- findInterval(x = age_months,
                      vec = breaks)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans
}


