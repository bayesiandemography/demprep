
## NO_TESTS
plot_date_to_age_group <- function(date, dob, breaks, open_last, labels, cex = 0.8) {
    old_par <- graphics::par(mar = c(6, 0, 0, 0),
                             mgp = c(0, 0, 0),
                             cex = cex)
    n_date <- length(date)
    n_br <- length(breaks)
    diff_br <- diff(breaks)
    date_min <- min(dob, na.rm = TRUE)
    date_max <- max(date, na.rm = TRUE)
    width_date <- date_max - date_min
    age <- (date - dob) / 365.25
    age_min <- min(breaks, age)
    age_max <- max(breaks, age)
    if (open_last)
        age_max <- age_max + diff_br[[n_br - 1L]]
    x_plot <- c(date_min - 0.15 * width_date, date_max)
    y_plot <- c(age_min, age_max)
    ## empty plotting frame
    plot(x = x_plot,
         y = y_plot,
         pch = NA,
         axes = FALSE,
         ylab = "",
         xlab = "")
    ## horizontal lines to show boundaries between age groups
    graphics::segments(x0 = rep(date_min, times = n_br),
                       y0 = breaks,
                       x1 = rep(date_max, times = n_br),
                       y1 = breaks,
                       lty = "solid")
    ## labels for boundaries between age groups
    graphics::text(x = date_min - 0.02 * width_date,
                   y = breaks,
                   labels = breaks,
                   cex = 0.7)
    ## labels for age groups
    graphics::text(x = date_min - 0.04 * width_date,
                   y = breaks[-n_br] + 0.5 * diff_br,
                   labels = sprintf('"%s"', labels[seq_len(n_br - 1L)]),
                   pos = 2,
                   cex = 0.9)
    if (open_last)
        graphics::text(x = date_min - 0.04 * width_date,
                       y = breaks[[n_br]] + 0.5 * diff_br[[n_br - 1L]],
                       labels = sprintf('"%s"', labels[[n_br]]),
                       pos = 2,
                       cex = 0.9)
    ## points for 'dob'
    graphics::points(x = dob,
                     y = rep(0, times = n_date),
                     pch = 19)    
    ## labels for 'dob'
    graphics::mtext(text = dob,
                    side = 1,
                    line = -0.5,
                    at = dob,
                    cex = 0.6,
                    las = 3)
    ## points for 'date'
    graphics::points(x = date,
                     y = age,
                     pch = 19)
    ## labels for 'date'
    graphics::mtext(text = date,
                    side = 1,
                    line = -0.5,
                    at = date,
                    cex = 0.6,
                    las = 3)
    ## life lines
    graphics::segments(x0 = dob,
                       y0 = rep(0, times = n_date),
                       x1 = date,
                       y1 = age,
                       lty = "dashed")
    ## xlab
    graphics::mtext(text = "Time",
                    side = 1,
                    line = 4,
                    cex = 0.7)
    graphics::par(old_par)
    invisible(NULL)
    ## ylab
    graphics::mtext(text = "Age",
                    side = 2,
                    line = 2,
                    las = 3,
                    cex = 0.7)
    graphics::par(old_par)
    invisible(NULL)
}


## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_group_year'
#'
#' @param date Dates of events or measurements.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class
#' \code{Date} using function \code{\link[base]{as.Date}}.
#' @param dob Dates of birth.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class
#' \code{Date} using function \code{\link[base]{as.Date}}.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 100.
#' @param open_last Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#'
#' @return If \code{as_factor} is \code{TRUE}, then the return
#' value is a factor; otherwise it is a character vector.
#' The length of the return value equals the length
#' of \code{date} or the length of \code{dob}, whichever
#' is greater.
#'
#' @seealso \code{\link{date_to_age_group_year}}
#'
#' @examples
#' plot_date_to_age_group_year(date = c("2002-11-09", "2004-04-27"),
#'                             dob = c("2000-07-13", "2001-03-21"),
#'                             break_max = 2)
#' plot_date_to_age_group_year(date = c("2002-11-09", "2004-04-27"),
#'                             dob = c("2000-07-13", "2001-03-21"),
#'                             break_max = 4,
#'                             open_last = FALSE)
#'
#' ## replicate date of birth
#' plot_date_to_age_group_year(date = c("2022-11-09", "2024-03-27"),
#'                             dob = "2011-05-18",
#'                             break_max = 15)
#'
#' ## return non-factor
#' plot_date_to_age_group_year(date = c("2024-03-27", "2022-11-09"),
#'                        dob = "2011-05-18")
#'
#' ## alternative specifications for oldest age group
#' plot_date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01")
#' plot_date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = 80)
#' plot_date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = NULL)
#' plot_date_to_age_group_year(date = "2019-09-22",
#'                        dob = "1910-01-01",
#'                        break_max = NULL,
#'                        open_last = FALSE)
#' @export
plot_date_to_age_group_year <- function(date,
                                        dob,
                                        break_max = 100,
                                        open_last = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    demcheck::err_has_non_na(x = date,
                             name = "date")
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## get age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## if final interval not open, check that all
    ## ages less than 'break_max'
    if (!open_last && !is.null(break_max))
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    ## make breaks
    breaks <- make_breaks_integer_year(age = age_years,
                                       width = 1L,
                                       break_max = break_max,
                                       open_last = open_last)
    ## make labels for these breaks
    labels <- make_labels_age_group(breaks = breaks,
                                    open_last = open_last,
                                    include_na = FALSE)
    ## make plot
    plot_date_to_age_group(date = date,
                           dob = dob,
                           breaks = breaks,
                           open_last = open_last,
                           labels = labels)
}

#' ## HAS_TESTS
#' #' Convert dates to multi-year age groups
#' #'
#' #' Given dates when events occurred or measurements were made,
#' #' together with dates of birth, allocate the events or
#' #' measurements to age groups. These
#' #' age groups all have the same width, except possibly
#' #' the final age group. The width is measured in years
#' #' and must be an integer.
#' #'
#' #' \code{date} and \code{dob} must have the same length,
#' #' unless one of them has length 1, in which case the
#' #' length-1 argument is recycled.
#' #'
#' #' \code{break_max} and \code{open_last} are used to specify
#' #' the oldest age group.
#' #' If \code{break_max} is non-\code{NULL} and
#' #' \code{open_last} is \code{TRUE}, the oldest
#' #' age group is \code{[break_max, Inf)} years. If
#' #' \code{break_max} is non-\code{NULL} and 
#' #' \code{open_last} is \code{FALSE}, the oldest age
#' #' group is \code{[break_max-width, break_max)} years.
#' #' If \code{break_max} is \code{NULL}, the oldest
#' #' age group is derived from the data.
#' #'
#' #' When \code{as_factor} is \code{TRUE} the levels of
#' #' the factor include all intermediate age groups,
#' #' including age groups that not appear in the data.
#' #'
#' #' @inheritParams date_to_age_group_year
#' #' @param width The width in years of the age intervals.
#' #' A positive integer. Defaults to 5.
#' #'
#' #' @return If \code{as_factor} is \code{TRUE}, then the
#' #' return value is a factor; otherwise it is a character vector.
#' #' The length of the return value equals the length
#' #' of \code{date} or the length of \code{dob}, whichever
#' #' is greater.
#' #'
#' #' @seealso Other functions for creating age groups are
#' #' \code{\link{date_to_age_group_year}},
#' #' \code{\link{date_to_age_group_lifetab}},
#' #' \code{\link{date_to_age_group_births}},
#' #' \code{\link{date_to_age_group_custom}},
#' #' \code{\link{date_to_age_group_quarter}},
#' #' and \code{\link{date_to_age_group_month}}.
#' #' Other functions for working with multi-year intervals are
#' #' \code{\link{date_to_period_multi}},
#' #' \code{\link{date_to_cohort_multi}},
#' #' and \code{\link{date_to_triangle_multi}}.
#' #' See \code{\link{make_labels_age_group}} for the rules
#' #' on constructing labels for age groups.
#' #'
#' #' @examples
#' #' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#' #'                         dob = c("2001-03-21", "2000-07-13"))
#' #'
#' #' ## alternative values for 'width'
#' #' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#' #'                         dob = c("2001-03-21", "2000-07-13"),
#' #'                         width = 10)
#' #' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#' #'                         dob = c("2001-03-21", "2000-07-13"),
#' #'                         width = 1)
#' #'
#' #' ## replicate date of birth
#' #' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#' #'                         dob = "2011-05-18")
#' #'
#' #' ## return non-factor
#' #' date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#' #'                         dob = "2011-05-18",
#' #'                         as_factor = FALSE)
#' #'
#' #' ## alternative specifications for oldest age group
#' #' date_to_age_group_multi(date = "2019-09-22",
#' #'                         dob = "1910-01-01")
#' #' date_to_age_group_multi(date = "2019-09-22",
#' #'                         dob = "1910-01-01",
#' #'                         break_max = 80)
#' #' date_to_age_group_multi(date = "2019-09-22",
#' #'                         dob = "1910-01-01",
#' #'                         break_max = NULL)
#' #' date_to_age_group_multi(date = "2019-09-22",
#' #'                         dob = "1910-01-01",
#' #'                         break_max = NULL,
#' #'                         open_last = FALSE)
#' #' @export
#' date_to_age_group_multi <- function(date,
#'                                     dob,
#'                                     width = 5,
#'                                     break_max = 100,
#'                                     open_last = TRUE,
#'                                     as_factor = TRUE) {
#'     ## Check arguments and/or apply defaults.
#'     ## Note that 'err_tdy_date_dob' enforces length >= 1
#'     l <- demcheck::err_tdy_date_dob(date = date,
#'                                     dob = dob)
#'     date <- l$date
#'     dob <- l$dob
#'     width <- demcheck::err_tdy_positive_integer_scalar(x = width,
#'                                                        name = "width")
#'     break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
#'                                                            name = "break_max",
#'                                                            null_ok = TRUE)
#'     if (!is.null(break_max))
#'         demcheck::err_is_logical_flag(x = open_last,
#'                                       name = "open_last")
#'     demcheck::err_is_logical_flag(x = as_factor,
#'                                   name = "as_factor")
#'     demcheck::err_multiple_of(x1 = break_max,
#'                               x2 = width,
#'                               name1 = "break_max",
#'                               name2 = "width",
#'                               null_ok = TRUE)
#'     ## deal with "empty" case where
#'     ## all date-dob pairs have NA
#'     ## and no 'break_max' supplied
#'     n_date <- length(date)
#'     all_empty <- all(is.na(date) | is.na(dob))
#'     if (all_empty && is.null(break_max)) {
#'         ans <- rep(NA_character_, times = n_date)
#'         if (as_factor)
#'             ans <- factor(ans)
#'         return(ans)
#'     }
#'     ## get age in months and years
#'     age_months <- age_completed_months(date = date,
#'                                        dob = dob)
#'     age_years <- age_months %/% 12L
#'     ## if final interval not open, check that all
#'     ## ages less than 'break_max'
#'     if (!open_last)
#'         demcheck::err_lt_break_max_age(age = age_years,
#'                                        break_max = break_max,
#'                                        date = date,
#'                                        dob = dob,
#'                                        unit = "year")    
#'     ## make breaks
#'     breaks <- make_breaks_integer_year(age = age_years,
#'                                        width = width,
#'                                        break_max = break_max,
#'                                        open_last = open_last)
#'     ## make labels for these breaks
#'     labels <- make_labels_age_group(breaks = breaks,
#'                                     open_last = open_last)
#'     ## assign labels to ages
#'     i <- findInterval(x = age_years,
#'                       vec = breaks)
#'     ans <- labels[i]
#'     ## return result
#'     if (as_factor)
#'         ans <- factor(x = ans,
#'                       levels = labels)
#'     ans
#' }
#' 
#' ## HAS_TESTS
#' #' Convert dates to age groups used in abridged life table
#' #'
#' #' Given dates of death and dates of birth,
#' #' allocate the deaths to age groups. These
#' #' age groups are the ones typically used in
#' #' "abridged" (ie not single-year) life tables: "0", "1-4",
#' #' and "5-9", "10-14", "10-14", and so on up to the
#' #' highest age group.
#' #'
#' #' \code{date} and \code{dob} must have the same length,
#' #' unless one of them has length 1, in which case the
#' #' length-1 argument is recycled.
#' #'
#' #' \code{break_max} is used to specify
#' #' the oldest age group, which is always open
#' #' on the right.
#' #' If \code{break_max} is \code{NULL}, the oldest
#' #' age group is derived from the data.
#' #'
#' #' When \code{as_factor} is \code{TRUE} the levels of
#' #' the factor include all intermediate age groups,
#' #' including age groups that not appear in the data.
#' #'
#' #' @inheritParams date_to_age_group_year
#' #' @param date Date of death.
#' #'
#' #' @return If \code{as_factor} is \code{TRUE}, then the return
#' #' value is a factor; otherwise it is a character vector.
#' #' The length of the return value equals the length
#' #' of \code{date} or the length of \code{dob}, whichever
#' #' is greater.
#' #'
#' #' @seealso Other functions for creating age groups are
#' #' \code{\link{date_to_age_group_year}},
#' #' \code{\link{date_to_age_group_multi}},
#' #' \code{\link{date_to_age_group_births}},
#' #' \code{\link{date_to_age_group_custom}},
#' #' \code{\link{date_to_age_group_quarter}},
#' #' and \code{\link{date_to_age_group_month}}.
#' #' See \code{\link{make_labels_age_group}} for the rules
#' #' on constructing labels for age groups.
#' #'
#' #' @examples
#' #' date_to_age_group_lifetab(date = c("2024-03-27", "2022-11-09"),
#' #'                           dob = c("2001-03-21", "2000-07-13"))
#' #'
#' #' ## replicate date of birth
#' #' date_to_age_group_lifetab(date = c("2024-03-27", "2022-11-09"),
#' #'                           dob = "2011-05-18")
#' #'
#' #' ## return non-factor
#' #' date_to_age_group_lifetab(date = c("2024-03-27", "2022-11-09"),
#' #'                           dob = "2011-05-18",
#' #'                           as_factor = FALSE)
#' #'
#' #' ## alternative specifications for oldest age group
#' #' date_to_age_group_lifetab(date = "2019-09-22",
#' #'                           dob = "1910-01-01")
#' #' date_to_age_group_lifetab(date = "2019-09-22",
#' #'                           dob = "1910-01-01",
#' #'                           break_max = 80)
#' #' @export
#' date_to_age_group_lifetab <- function(date, dob,
#'                                       break_max = 100,
#'                                       as_factor = TRUE) {
#'     ## Check arguments and/or apply defaults.
#'     ## Note that 'err_tdy_date_dob' enforces length >= 1
#'     l <- demcheck::err_tdy_date_dob(date = date,
#'                                     dob = dob)
#'     date <- l$date
#'     dob <- l$dob
#'     break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
#'                                                            name = "break_max",
#'                                                            null_ok = FALSE)
#'     demcheck::err_multiple_of_n(x = break_max,
#'                                 name = "break_max",
#'                                 n = 5L,
#'                                 null_ok = FALSE)
#'     demcheck::err_is_logical_flag(x = as_factor,
#'                                   name = "as_factor")
#'     ## deal with "empty" case where
#'     ## all date-dob pairs have NA, and
#'     ## we aren't making factors
#'     n_date <- length(date)
#'     all_empty <- all(is.na(date) | is.na(dob))
#'     if (all_empty && !as_factor) {
#'         ans <- rep(NA_character_, times = n_date)
#'         return(ans)
#'     }
#'     ## get age in months and years
#'     age_months <- age_completed_months(date = date,
#'                                        dob = dob)
#'     age_years <- age_months %/% 12L
#'     ## make breaks
#'     breaks <- make_breaks_integer_lifetab(break_max)
#'     ## make labels for breaks
#'     labels <- make_labels_age_group(breaks = breaks,
#'                                     open_last = TRUE,
#'                                     include_na = FALSE)
#'     ## assign labels to ages
#'     i <- findInterval(x = age_years,
#'                       vec = breaks)
#'     ans <- labels[i]
#'     ## return result
#'     if (as_factor)
#'         ans <- factor(x = ans,
#'                       levels = labels)
#'     ans
#' }
#' 
#' ## HAS_TESTS
#' #' Convert dates to age groups when measuring fertility 
#' #'
#' #' Given the dates when births occur,
#' #' and the dates of birth of the mothers,
#' #' allocate the births to age groups. These age groups all
#' #' have the same width, which is measured in years,
#' #' and in an integer.
#' #'
#' #' \code{date} and \code{dob} must have the same length,
#' #' unless one of them has length 1, in which case the
#' #' length-1 argument is recycled.
#' #'
#' #' \code{break_min} and \code{break_max} specify
#' #' the range of ages over which reproduction
#' #' is assumed to occur. If, for instance,
#' #' \code{break_min} is \code{15} and \code{break_max}
#' #' is \code{50}, all births are assumed to
#' #' occur to women aged 15 to 49 (inclusive).
#' #'
#' #' Datasets sometimes contain a few births to mothers
#' #' younger than the assumed minimum age of reproduction,
#' #' or births to mothers older than the assumed maximum age
#' #' of reproduction. Demographers often recode such births,
#' #' so that ones to unexpectedly young mothers are
#' #' treated as occurring just above the minimum age
#' #' for reproduction, and ones to unexpectedly old mothers
#' #' are treated as occurring just below the maximum
#' #' age for reproduction. This recoding can be justified
#' #' on the grounds that some of the original ages may have
#' #' been misreported, but it also alleviates any problems
#' #' with tabulations having small counts at extreme ages.
#' #' Recoding of mothers' ages outside the expected range
#' #' is controlled by parameters \code{recode_up}
#' #' and \code{recode_down}. The default
#' #' is for no recoding to occur.
#' #'
#' #' @inheritParams date_to_age_group_year
#' #' @param date Dates when births being measured occur.
#' #' @param dob Dates of birth of monthers.
#' #' @param break_min An integer or \code{NULL}.
#' #' Defaults to 15.
#' #' @param break_max An integer or \code{NULL}.
#' #' Defaults to 50.
#' #' @param width The width in years of the age intervals.
#' #' A positive integer defaulting to 5.
#' #' @param recode_up If \code{TRUE}, births to women
#' #' aged less than \code{break_min} are treated as occurring to
#' #' women in the lowest repoductive age group.
#' #' @param recode_down If \code{TRUE}, births to women
#' #' aged \code{break_max} or more are treated as
#' #' occurring to women in the highest reproductive
#' #' age group.
#' #'
#' #' @return If \code{as_factor} is \code{TRUE}, then the return
#' #' value is a factor; otherwise it is a character vector.
#' #' The length of the return value equals the length
#' #' of \code{date} or the length of \code{dob}, whichever
#' #' is greater.
#' #'
#' #' @seealso Other functions for creating age groups are
#' #' \code{\link{date_to_age_group_year}},
#' #' \code{\link{date_to_age_group_multi}},
#' #' \code{\link{date_to_age_group_lifetab}},
#' #' \code{\link{date_to_age_group_custom}},
#' #' \code{\link{date_to_age_group_quarter}},
#' #' and \code{\link{date_to_age_group_month}}.
#' #' See \code{\link{make_labels_age_group}} for the rules
#' #' on constructing labels for age groups.
#' #'
#' #' @examples
#' #' date_to_age_group_births(date = c("2024-03-27", "2022-11-09"),
#' #'                        dob = c("2001-03-21", "2000-07-13"))
#' #'
#' #' ## alternative values for 'width'
#' #' date_to_age_group_births(date = c("2024-03-27", "2022-11-09"),
#' #'                        dob = c("2001-03-21", "2000-07-13"),
#' #'                        width = 10,
#' #'                        break_min = 20)
#' #' date_to_age_group_births(date = c("2024-03-27", "2022-11-09"),
#' #'                        dob = c("2001-03-21", "2000-07-13"),
#' #'                        width = 1)
#' #'
#' #' ## replicate date of birth
#' #' date_to_age_group_births(date = c("2024-03-27", "2022-11-09"),
#' #'                        dob = "2001-05-18")
#' #'
#' #' ## return non-factor
#' #' date_to_age_group_births(date = c("2024-03-27", "2022-11-09"),
#' #'                        dob = "2001-05-18",
#' #'                        as_factor = FALSE)
#' #'
#' #' ## allow youngest and oldest age groups to be
#' #' ## set by the data
#' #' date_to_age_group_births(date = c("2052-01-02", "2019-09-22", "2022-10-08"),
#' #'                        dob = c("2000-01-01", "2001-03-17", "2010-07-05"),
#' #'                        break_min = NULL,
#' #'                        break_max = NULL)
#' #'
#' #' ## recode ages outside the expected range
#' #' date_to_age_group_births(date = c("2052-01-02", "2019-09-22", "2022-10-08"),
#' #'                        dob = c("2000-01-01", "2001-03-17", "2010-07-05"),
#' #'                        recode_up = TRUE,
#' #'                        recode_down = TRUE)
#' #' @export
#' date_to_age_group_births <- function(date, dob,
#'                                    break_min = 15,
#'                                    break_max = 50,
#'                                    width = 5,
#'                                    recode_up = FALSE,
#'                                    recode_down = FALSE,
#'                                    as_factor = TRUE) {
#'     ## Check arguments and/or apply defaults.
#'     ## Note that 'err_tdy_date_dob' enforces length >= 1
#'     l <- demcheck::err_tdy_date_dob(date = date,
#'                                     dob = dob)
#'     date <- l$date
#'     dob <- l$dob
#'     break_min <- demcheck::err_tdy_positive_integer_scalar(x = break_min,
#'                                                            name = "break_min",
#'                                                            null_ok = TRUE)
#'     break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
#'                                                            name = "break_max",
#'                                                            null_ok = TRUE)
#'     width <- demcheck::err_tdy_positive_integer_scalar(x = width,
#'                                                        name = "width")
#'     if (!is.null(break_min) && !is.null(break_max)) {
#'         demcheck::err_gt_scalar(x1 = break_max,
#'                                    x2 = break_min,
#'                                    name1 = "break_max",
#'                                    name2 = "break_min")
#'         if ((break_max - break_min) %% width != 0L)
#'             stop(gettextf("difference between '%s' [%d] and '%s' [%d] not divisible by '%s' [%d]",
#'                           "break_max", break_max, "break_min", break_min, "width", width))
#'     }
#'     demcheck::err_is_logical_flag(x = recode_up,
#'                                   name = "recode_up")
#'     demcheck::err_is_logical_flag(x = recode_down,
#'                                   name = "recode_down")
#'     ## deal with "empty" case where
#'     ## all date-dob pairs have NA, and
#'     ## we aren't making factors
#'     n_date <- length(date)
#'     all_empty <- all(is.na(date) | is.na(dob))
#'     if (all_empty && !as_factor) {
#'         ans <- rep(NA_character_, times = n_date)
#'         return(ans)
#'     }
#'     ## get age in months and years
#'     age_months <- age_completed_months(date = date,
#'                                        dob = dob)
#'     age_years <- age_months %/% 12L
#'     ## check that ages lie within limits implied by 'break_min' and 'break_max'
#'     if (!is.null(break_min)) {
#'         is_lt_min <- age_years < break_min
#'         i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
#'         if (i_lt_min > 0L) {
#'             if (recode_up)
#'                 age_years[is_lt_min] <- break_min
#'             else {
#'                 stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
#'                                     "but 'break_min' is %d and 'recode_up' is FALSE"),
#'                               date[[i_lt_min]],
#'                               dob[[i_lt_min]],
#'                               age_years[[i_lt_min]],
#'                               break_min))
#'             }
#'         }
#'     }
#'     if (!is.null(break_max)) {
#'         is_ge_max <- age_years >= break_max
#'         i_ge_max <- match(TRUE, is_ge_max, nomatch = 0L)
#'         if (i_ge_max > 0L) {
#'             if (recode_down)
#'                 age_years[is_ge_max] <- break_max - 1L
#'             else {
#'                 stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
#'                                     "but 'break_max' is %d and 'recode_down' is FALSE"),
#'                               date[[i_ge_max]],
#'                               dob[[i_ge_max]],
#'                               age_years[[i_ge_max]],
#'                               break_max))
#'             }
#'         }
#'     }
#'     ## make breaks
#'     breaks <- make_breaks_integer_births(age = age_years,
#'                                        width = width,
#'                                        break_min = break_min,
#'                                        break_max = break_max)
#'     ## make labels for breaks
#'     labels <- make_labels_age_group(breaks = breaks,
#'                                     open_last = FALSE)
#'     ## assign labels to ages
#'     i <- findInterval(x = age_years,
#'                       vec = breaks)
#'     ans <- labels[i]
#'     ## return result
#'     if (as_factor)
#'         ans <- factor(x = ans,
#'                       levels = labels)
#'     ans
#' }
#' 
#' ## HAS_TESTS
#' #' Convert dates to customized age groups
#' #'
#' #' Given dates when events occurred or measurements were made,
#' #' and dates of birth, allocate the events or measurements
#' #' to age groups. \code{date_to_age_group_custom} is the most flexible
#' #' of the \code{date_to_age_group} functions
#' #' in that the age groups can have any combination of widths,
#' #' though the widths must be defined in whole numbers of years.
#' #'
#' #' \code{date} and \code{dob} must have the same length,
#' #' unless one of them has length 1, in which case the
#' #' length-1 argument is recycled.
#' #'
#' #' \code{breaks} is used to specify the points at which
#' #' each age group starts and finishes. If 
#' #' \code{open_last} is \code{TRUE}, and \code{b} is
#' #' the last value for \code{breaks}, then the oldest
#' #' age group is \code{[b, Inf)} years. 
#' #' If \code{open_last} is \code{FALSE}, \code{a} is the
#' #' second-to-last value for \code{breaks} and \code{b}
#' #' is the last value, then the oldest age
#' #' group is \code{[a, b)} years.
#' #'
#' #' When \code{as_factor} is \code{TRUE} the levels of
#' #' the factor include all intermediate age groups,
#' #' including age groups that not appear in the data.
#' #'
#' #' @inheritParams date_to_age_group_year
#' #' @param breaks A vector of strictly increasing integer values.
#' #'
#' #' @return If \code{as_factor} is \code{TRUE}, then the
#' #' return value is a factor; otherwise it is a character vector.
#' #' The length of the return value equals the length
#' #' of \code{date} or the length of \code{dob}, whichever
#' #' is greater.
#' #'
#' #' @seealso Other functions for creating age groups are
#' #' \code{\link{date_to_age_group_year}},
#' #' \code{\link{date_to_age_group_multi}},
#' #' \code{\link{date_to_age_group_lifetab}},
#' #' \code{\link{date_to_age_group_births}},
#' #' \code{\link{date_to_age_group_quarter}},
#' #' and \code{\link{date_to_age_group_month}}.
#' #' See \code{\link{make_labels_age_group}} for the rules
#' #' on constructing labels for age groups.
#' #'
#' #' @examples
#' #' date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#' #'                          dob = c("2001-03-21", "2000-07-13"),
#' #'                          breaks = c(0, 15, 60))
#' #' date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#' #'                          dob = c("2001-03-21", "2000-07-13"),
#' #'                          breaks = c(15, 40, 65))
#' #'
#' #' ## replicate date of birth
#' #' date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#' #'                          dob = "2001-01-01",
#' #'                          breaks = c(0, 15, 60))
#' #'
#' #' ## return non-factor
#' #' date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#' #'                          dob = "2001-01-01",
#' #'                          breaks = c(0, 15, 60),
#' #'                          as_factor = FALSE)
#' #'
#' #' ## alternative specifications for oldest age group
#' #' date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#' #'                          dob = c("2001-03-21", "2000-07-13"),
#' #'                          breaks = c(15, 65, 100))
#' #' date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#' #'                          dob = c("2001-03-21", "2000-07-13"),
#' #'                          breaks = c(15, 65, 100),
#' #'                          open_last = FALSE)
#' #' @export
#' date_to_age_group_custom <- function(date, dob,
#'                                      breaks = NULL,
#'                                      open_last = TRUE,
#'                                      as_factor = TRUE) {
#'     ## Check arguments and/or apply defaults.
#'     ## Note that 'err_tdy_date_dob' enforces length >= 1
#'     l <- demcheck::err_tdy_date_dob(date = date,
#'                                     dob = dob)
#'     date <- l$date
#'     dob <- l$dob
#'     breaks <- demcheck::err_tdy_breaks_integer_age(breaks = breaks,
#'                                                open_last = open_last)
#'     demcheck::err_is_logical_flag(x = open_last,
#'                                   name = "open_last")
#'     demcheck::err_is_logical_flag(x = as_factor,
#'                                   name = "as_factor")
#'     ## deal with "empty" case where 'breaks' has length 0
#'     all_empty <- all(is.na(date) | is.na(dob))
#'     n_break <- length(breaks)
#'     n_date <- length(date)
#'     if (n_break == 0L) {
#'         if (all_empty) {
#'             ans <- rep(NA_character_, times = n_date)
#'             if (as_factor)
#'                 ans <- factor(ans)
#'             return(ans)
#'         }
#'         else
#'             stop(gettextf("'%s' has length %d",
#'                           "breaks", 0L))
#'     }
#'     ## deal with "empty" case where
#'     ## all date-dob pairs have NA, and
#'     ## we aren't making factors
#'     if (all_empty && !as_factor) {
#'         ans <- rep(NA_character_, times = n_date)
#'         return(ans)
#'     }
#'     ## get age in months and years
#'     age_months <- age_completed_months(date = date,
#'                                        dob = dob)
#'     age_years <- age_months %/% 12L
#'     ## check that ages lie within limits implied by 'breaks' and 'open_last'
#'     is_lt_min <- age_years < breaks[[1L]]
#'     i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
#'     if (i_lt_min > 0L) {
#'         stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
#'                             "but minimum value for '%s' is %d"),
#'                       date[[i_lt_min]],
#'                       dob[[i_lt_min]],
#'                       age_years[[i_lt_min]],
#'                       "breaks",
#'                       breaks[[1L]]))
#'     }
#'     if (!open_last) {
#'         is_ge_max <- age_years >= breaks[[n_break]]
#'         i_ge_max <- match(TRUE, is_ge_max, nomatch = 0L)
#'         if (i_ge_max > 0L) {
#'             stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
#'                                 "but 'open_last' is FALSE and maximum value for 'breaks' is %d"),
#'                           date[[i_ge_max]],
#'                           dob[[i_ge_max]],
#'                           age_years[[i_ge_max]],
#'                           breaks[[n_break]]))
#'         }
#'     }
#'     ## make labels for breaks
#'     labels <- make_labels_age_group(breaks = breaks,
#'                                     open_last = open_last,
#'                                     include_na = FALSE)
#'     ## assign labels to ages
#'     i <- findInterval(x = age_years,
#'                       vec = breaks)
#'     ans <- labels[i]
#'     ## return result
#'     if (as_factor)
#'         ans <- factor(x = ans,
#'                       levels = labels)
#'     ans
#' }
#' 
#' ## HAS_TESTS
#' #' Convert dates to one-quarter age groups
#' #'
#' #' Given dates when events occurred or measurements were made,
#' #' and dates of birth, allocate the events or measurements
#' #' to age groups. These
#' #' age groups all have widths of one quarter (ie three months),
#' #' except possibly the final age group.
#' #'
#' #' A person belongs to age group \code{"a"} if that
#' #' person was exactly \code{a} quarters
#' #' old at their most recent birthday. For instance, a person
#' #' who had their fifth birthday two days ago belongs to age
#' #' group \code{"20q"} and a person who was born (had their zero-th
#' #' birthday) three months ago belongs to age group \code{"0q"}.
#' #'
#' #' \code{date} and \code{dob} must have the same length,
#' #' unless one of them has length 1, in which case the
#' #' length-1 argument is recycled.
#' #'
#' #' \code{break_max} and \code{open_last} are used to specify
#' #' the oldest age group.
#' #' If \code{break_max} is non-\code{NULL} and
#' #' \code{open_last} is \code{TRUE}, the oldest
#' #' age group is \code{[break_max, Inf)} quarters. if
#' #' \code{break_max} is non-\code{NULL} and 
#' #' \code{open_last} is \code{FALSE}, the oldest age
#' #' group is \code{[break_max-1, break_max)} quarters.
#' #' If \code{break_max} is \code{NULL}, the oldest
#' #' age group is derived from the data.
#' #'
#' #' When \code{as_factor} is \code{TRUE} the levels of
#' #' the factor include all intermediate age groups,
#' #' including age groups that not appear in the data.
#' #'
#' #' @param date Dates of events or measurements.
#' #' @param dob Dates of birth.
#' #' @param break_max An integer or \code{NULL}.
#' #' Defaults to 400.
#' #' @param open_last Whether the final age group
#' #' has no upper limit. Defaults to \code{TRUE}.
#' #' @param as_factor Whether the return value is a factor.
#' #' Defaults to \code{TRUE}.
#' #'
#' #' @return If \code{as_factor} is \code{TRUE}, then the return
#' #' value is a factor; otherwise it is a character vector.
#' #' The length of the return value equals the length
#' #' of \code{date} or the length of \code{dob}, whichever
#' #' is greater.
#' #'
#' #' @seealso Other functions for creating age groups are
#' #' \code{\link{date_to_age_group_year}},
#' #' \code{\link{date_to_age_group_multi}},
#' #' \code{\link{date_to_age_group_lifetab}},
#' #' \code{\link{date_to_age_group_births}},
#' #' \code{\link{date_to_age_group_custom}},
#' #' and \code{\link{date_to_age_group_month}}.
#' #' Other functions for working with one-quarter intervals are
#' #' \code{\link{date_to_period_quarter}},
#' #' \code{\link{date_to_cohort_quarter}},
#' #' and \code{\link{date_to_triangle_quarter}}.
#' #' See \code{\link{make_labels_age_group_quarter}} for the rules
#' #' on constructing labels for age groups.
#' #'
#' #' @examples
#' #' date_to_age_group_quarter(date = c("2024-03-27", "2022-11-09"),
#' #'                           dob = c("2001-03-21", "2000-07-13"))
#' #'
#' #' ## replicate date of birth
#' #' date_to_age_group_quarter(date = c("2024-03-27", "2022-11-09"),
#' #'                           dob = "2011-05-18")
#' #'
#' #' ## return non-factor
#' #' date_to_age_group_quarter(date = c("2024-03-27", "2022-11-09"),
#' #'                           dob = "2011-05-18",
#' #'                           as_factor = FALSE)
#' #'
#' #' ## alternative specifications for oldest age group
#' #' date_to_age_group_quarter(date = "2019-09-22",
#' #'                           dob = "1910-01-01")
#' #' date_to_age_group_quarter(date = "2019-09-22",
#' #'                           dob = "1910-01-01",
#' #'                           break_max = 320)
#' #' date_to_age_group_quarter(date = "2019-09-22",
#' #'                           dob = "1910-01-01",
#' #'                           break_max = NULL)
#' #' date_to_age_group_quarter(date = "2019-09-22",
#' #'                           dob = "1910-01-01",
#' #'                           break_max = NULL,
#' #'                           open_last = FALSE)
#' #' @export
#' date_to_age_group_quarter <- function(date,
#'                                       dob,
#'                                       break_max = 400,
#'                                       open_last = TRUE,
#'                                       as_factor = TRUE) {
#'     ## Check arguments and/or apply defaults.
#'     ## Note that 'err_tdy_date_dob' enforces length >= 1
#'     l <- demcheck::err_tdy_date_dob(date = date,
#'                                     dob = dob)
#'     date <- l$date
#'     dob <- l$dob
#'     break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
#'                                                            name = "break_max",
#'                                                            null_ok = TRUE)
#'     demcheck::err_is_logical_flag(x = open_last,
#'                                   name = "open_last")
#'     demcheck::err_is_logical_flag(x = as_factor,
#'                                   name = "as_factor")
#'     ## deal with "empty" case where
#'     ## all date-dob pairs have NA
#'     ## and no 'break_max' supplied
#'     n_date <- length(date)
#'     all_empty <- all(is.na(date) | is.na(dob))
#'     if (all_empty && is.null(break_max)) {
#'         ans <- rep(NA_character_, times = n_date)
#'         if (as_factor)
#'             ans <- factor(ans)
#'         return(ans)
#'     }
#'     ## get age in months and quarters    
#'     age_months <- age_completed_months(date = date,
#'                                        dob = dob)
#'     age_quarters <- age_months %/% 3L
#'     ## if final interval not open, check that all
#'     ## ages less than 'break_max'
#'     if (!is.null(break_max) && !open_last)
#'         demcheck::err_lt_break_max_age(age = age_quarters,
#'                                        break_max = break_max,
#'                                        date = date,
#'                                        dob = dob,
#'                                        unit = "quarter")
#'     ## make breaks
#'     breaks <- make_breaks_integer_month_quarter(age = age_quarters,
#'                                                 break_max = break_max,
#'                                                 open_last = open_last)
#'     ## make labels for these breaks
#'     n_break <- length(breaks)
#'     break_max <- breaks[[n_break]]
#'     labels <- make_labels_age_group_quarter(break_min = 0L,
#'                                             break_max = break_max,
#'                                             open_last = open_last)
#'     ## assign labels to ages
#'     i <- findInterval(x = age_quarters,
#'                       vec = breaks)
#'     ans <- labels[i]
#'     ## return result
#'     if (as_factor)
#'         ans <- factor(x = ans,
#'                       levels = labels)
#'     ans
#' }
#' 
#' ## HAS_TESTS
#' #' Convert dates to one-month age groups
#' #'
#' #' Given dates when events occurred or measurements were made,
#' #' and dates of birth, allocate the events or
#' #' measurements to age groups. These
#' #' age groups all have widths of one month,
#' #' except possibly the final age group.
#' #'
#' #' A person belongs to age group \code{"a"} if that
#' #' person was exactly \code{a} months
#' #' old at their most recent birthday. For instance, a person
#' #' who had their fifth birthday two days ago belongs to age
#' #' group \code{"60m"} and a person who was born (had their zero-th
#' #' birthday) three months ago belongs to age group \code{"60m"}.
#' #'
#' #' \code{date} and \code{dob} must have the same length,
#' #' unless one of them has length 1, in which case the
#' #' length-1 argument is recycled.
#' #'
#' #' \code{break_max} and \code{open_last} are used to specify
#' #' the oldest age group.
#' #' If \code{break_max} is non-\code{NULL} and
#' #' \code{open_last} is \code{TRUE}, the oldest
#' #' age group is \code{[break_max, Inf)} months. If
#' #' \code{break_max} is non-\code{NULL} and 
#' #' \code{open_last} is \code{FALSE}, the oldest age
#' #' group is \code{[break_max-1, break_max)} months.
#' #' If \code{break_max} is \code{NULL}, the oldest
#' #' age group is derived from the data.
#' #'
#' #' When \code{as_factor} is \code{TRUE} the levels of
#' #' the factor include all intermediate age groups,
#' #' including age groups that not appear in the data.
#' #'
#' #' @param date Dates of events or measurements.
#' #' @param dob Dates of birth.
#' #' @param break_max An integer or \code{NULL}.
#' #' Defaults to 1200.
#' #' @param open_last Whether the final age group
#' #' has no upper limit. Defaults to \code{TRUE}.
#' #' @param as_factor Whether the return value is a factor.
#' #' Defaults to \code{TRUE}.
#' #'
#' #' @return If \code{as_factor} is \code{TRUE}, then the return
#' #' value is a factor; otherwise it is a character vector.
#' #' The length of the return value equals the length
#' #' of \code{date} or the length of \code{dob}, whichever
#' #' is greater.
#' #'
#' #' @seealso Other functions for creating age groups are
#' #' \code{\link{date_to_age_group_year}},
#' #' \code{\link{date_to_age_group_multi}},
#' #' \code{\link{date_to_age_group_lifetab}},
#' #' \code{\link{date_to_age_group_births}},
#' #' \code{\link{date_to_age_group_custom}},
#' #' \code{\link{date_to_age_group_quarter}}.
#' #' Other functions for working with one-month intervals are
#' #' \code{\link{date_to_period_month}},
#' #' \code{\link{date_to_cohort_month}},
#' #' and \code{\link{date_to_triangle_month}}.
#' #' See \code{\link{make_labels_age_group_month}} for the rules
#' #' on constructing labels for age groups.
#' #'
#' #' @examples
#' #' date_to_age_group_month(date = c("2024-03-27", "2022-11-09"),
#' #'                         dob = c("2001-03-21", "2000-07-13"))
#' #'
#' #' ## replicate date of birth
#' #' date_to_age_group_month(date = c("2024-03-27", "2022-11-09"),
#' #'                         dob = "2011-05-18")
#' #'
#' #' ## return non-factor
#' #' date_to_age_group_month(date = c("2024-03-27", "2022-11-09"),
#' #'                         dob = "2011-05-18",
#' #'                         as_factor = FALSE)
#' #'
#' #' ## alternative specifications for oldest age group
#' #' date_to_age_group_month(date = "2019-09-22",
#' #'                         dob = "1910-01-01")
#' #' date_to_age_group_month(date = "2019-09-22",
#' #'                         dob = "1910-01-01",
#' #'                         break_max = 320)
#' #' date_to_age_group_month(date = "2019-09-22",
#' #'                         dob = "1910-01-01",
#' #'                         break_max = NULL)
#' #' date_to_age_group_month(date = "2019-09-22",
#' #'                         dob = "1910-01-01",
#' #'                         break_max = NULL,
#' #'                         open_last = FALSE)
#' #' @export
#' date_to_age_group_month <- function(date,
#'                                     dob,
#'                                     break_max = 1200,
#'                                     open_last = TRUE,
#'                                     as_factor = TRUE) {
#'     ## Check arguments and/or apply defaults.
#'     ## Note that 'err_tdy_date_dob' enforces length >= 1
#'     l <- demcheck::err_tdy_date_dob(date = date,
#'                                     dob = dob)
#'     date <- l$date
#'     dob <- l$dob
#'     break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
#'                                                            name = "break_max",
#'                                                            null_ok = TRUE)
#'     demcheck::err_is_logical_flag(x = open_last,
#'                                   name = "open_last")
#'     demcheck::err_is_logical_flag(x = as_factor,
#'                                   name = "as_factor")
#'     ## deal with "empty" case where
#'     ## all date-dob pairs have NA
#'     ## and no 'break_max' supplied
#'     n_date <- length(date)
#'     all_empty <- all(is.na(date) | is.na(dob))
#'     if (all_empty && is.null(break_max)) {
#'         ans <- rep(NA_character_, times = n_date)
#'         if (as_factor)
#'             ans <- factor(ans)
#'         return(ans)
#'     }
#'     ## get age in months
#'     age_months <- age_completed_months(date = date,
#'                                        dob = dob)
#'     ## if final interval not open, check that all
#'     ## ages less than 'break_max'
#'     if (!open_last && !is.null(break_max))
#'         demcheck::err_lt_break_max_age(age = age_months,
#'                                        break_max = break_max,
#'                                        date = date,
#'                                        dob = dob,
#'                                        unit = "month")
#'     ## make breaks
#'     breaks <- make_breaks_integer_month_quarter(age = age_months,
#'                                                 break_max = break_max,
#'                                                 open_last = open_last)
#'     ## make labels for these breaks
#'     n_break <- length(breaks)
#'     break_max <- breaks[[n_break]]
#'     labels <- make_labels_age_group_month(break_min = 0L,
#'                                           break_max = break_max,
#'                                           open_last = open_last,
#'                                           include_na = FALSE)
#'     ## assign labels to ages
#'     i <- findInterval(x = age_months,
#'                       vec = breaks)
#'     ans <- labels[i]
#'     ## return result
#'     if (as_factor)
#'         ans <- factor(x = ans,
#'                       levels = labels)
#'     ans
#' }


