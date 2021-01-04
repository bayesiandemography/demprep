
## NO_TESTS
plot_date_to_age_group <- function(date, dob, unit, breaks, open_last, labels,
                                   show_months, cex = 0.8) {
    old_par <- graphics::par(mar = c(6, 0, 0, 0),
                             mgp = c(0, 0, 0),
                             cex = cex)
    n_date <- length(date)
    days_per_unit <- switch(unit,
                            month = 31L,
                            quarter = 31L * 4L,
                            year = 31L * 12L,
                            stop("invalid unit"))
    n_br <- length(breaks)
    diff_br <- diff(breaks)
    date_min <- min(dob, na.rm = TRUE)
    date_min <- rollback(date_min)
    date_max <- max(date, na.rm = TRUE)
    date_max <- rollforward(date_max)
    width_date <- date_max - date_min
    age_approx <- as.integer(date - dob) %/% days_per_unit
    age_max <- max(breaks, age_approx)
    if (open_last)
        age_max <- age_max + diff(breaks)[n_br - 1L]
    x_plot <- c(date_min - 0.15 * width_date, date_max)
    y_plot <- c(0, age_max)
    ## empty plotting frame
    plot(x = x_plot,
         y = y_plot,
         pch = NA,
         axes = FALSE,
         ylab = "",
         xlab = "")
    ## vertical lines to show boundaries between months
    if (show_months) {
        boundaries_months <- seq(from = date_min,
                                 to = date_max,
                                 by = "month")
        graphics::segments(x0 = boundaries_months,
                           y0 = 0,
                           x1 = boundaries_months,
                           y1 = breaks[[n_br]],
                           col = "grey")
        graphics::mtext(text = dob,
                        side = 1,
                        line = -0.5,
                        at = boundaries_months,
                        cex = 0.6,
                        las = 3,
                        col = "grey")
    }
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
    ## life lines, and points for date
    for (i in seq_along(date)) {
        coord <- coord_lifeline(date1 = date[[i]],
                                dob1 = dob[[i]])
        x0 <- coord$x0
        y0 <- coord$y0 / days_per_unit
        x1 <- coord$x1
        y1 <- coord$y1 / days_per_unit
        graphics::segments(x0 = x0,
                           y0 = y0,
                           x1 = x1,
                           y1 = y1,
                           lty = "dashed")
        graphics::points(x = date[[i]],
                         y = y1[[length(y1)]],
                         pch = 19)
    }        
    ## labels for 'date'
    graphics::mtext(text = date,
                    side = 1,
                    line = -0.5,
                    at = date,
                    cex = 0.6,
                    las = 3)
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
                    las = 1,
                    cex = 0.7)
    graphics::par(old_par)
    invisible(NULL)
}


## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_group_year'
#'
#' Create plot illustrating the inputs and outputs
#' of function \code{\link{date_to_age_group_year}}.
#' 
#' This function is for learning about the
#' labelling conventions used in the \strong{dem} packages,
#' and about their implementation in package
#' \strong{demprep}. It would not normally be used
#' during actual data analysis.
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
#' @param show_months Whether to include vertical
#' lines showing boundaries between months.
#' Defaults to \code{FALSE}.
#'
#' @examples
#' plot_date_to_age_group_year(date = c("2002-11-09", "2004-04-27"),
#'                             dob = c("2000-07-13", "2001-03-21"),
#'                             break_max = 2)
#' plot_date_to_age_group_year(date = c("2002-11-09", "2004-04-27"),
#'                             dob = c("2000-07-13", "2001-03-21"),
#'                             break_max = 4,
#'                             open_last = FALSE)
#' @export
plot_date_to_age_group_year <- function(date,
                                        dob,
                                        break_max = 100,
                                        open_last = TRUE,
                                        show_months = FALSE) {
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
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
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
                           unit = "year",
                           breaks = breaks,
                           open_last = open_last,
                           labels = labels,
                           show_months = show_months)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_group_multi'
#'
#' Create plot illustrating the inputs and outputs
#' of function \code{\link{date_to_age_group_multi}}.
#' 
#' This function is for learning about the
#' labelling conventions used in the \strong{dem} packages,
#' and about their implementation in package
#' \strong{demprep}. It would not normally be used
#' during actual data analysis.
#'
#' @inheritParams plot_date_to_age_group_year
#' @param width The width in years of the age intervals.
#' A positive integer. Defaults to 5.
#'
#' @examples
#' plot_date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#'                              dob = c("2001-03-21", "2000-07-13"))
#'
#' ## alternative values for 'width'
#' plot_date_to_age_group_multi(date = c("2024-03-27", "2022-11-09"),
#'                              dob = c("2001-03-21", "2000-07-13"),
#'                              width = 10)
#'
#' ## alternative specifications for oldest age group
#' plot_date_to_age_group_multi(date = "2019-09-22",
#'                              dob = "1910-01-01",
#'                               width = 20)
#' plot_date_to_age_group_multi(date = "2019-09-22",
#'                              dob = "1910-01-01",
#'                              width = 20,
#'                              break_max = 80)
#' @export
plot_date_to_age_group_multi <- function(date,
                                         dob,
                                         width = 5,
                                         break_max = 100,
                                         open_last = TRUE,
                                         show_months = FALSE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    demcheck::err_has_non_na(x = date,
                             name = "date")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    if (!is.null(break_max))
        demcheck::err_is_logical_flag(x = open_last,
                                      name = "open_last")
    demcheck::err_multiple_of(x1 = break_max,
                              x2 = width,
                              name1 = "break_max",
                              name2 = "width",
                              null_ok = TRUE)
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    ## get age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## if final interval not open, check that all
    ## ages less than 'break_max'
    if (!open_last)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")    
    ## make breaks
    breaks <- make_breaks_integer_year(age = age_years,
                                       width = width,
                                       break_max = break_max,
                                       open_last = open_last)
    ## make labels for these breaks
    labels <- make_labels_age_group(breaks = breaks,
                                    open_last = open_last)

    ## make plot
    plot_date_to_age_group(date = date,
                           dob = dob,
                           unit = "year",
                           breaks = breaks,
                           open_last = open_last,
                           labels = labels,
                           show_months = show_months)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_group_lifetab'
#'
#' Create plot illustrating the inputs and outputs
#' of function \code{\link{date_to_age_group_lifetab}}.
#' 
#' This function is for learning about the
#' labelling conventions used in the \strong{dem} packages,
#' and about their implementation in package
#' \strong{demprep}. It would not normally be used
#' during actual data analysis.
#'
#' @inheritParams plot_date_to_age_group_year
#' @param date Date of death.
#'
#' @examples
#' plot_date_to_age_group_lifetab(date = c("2024-03-27", "2022-11-09"),
#'                                dob = c("2001-03-21", "2000-07-13"),
#'                                break_max = 30)
#' @export
plot_date_to_age_group_lifetab <- function(date,
                                           dob,
                                           break_max = 100,
                                           show_months = FALSE) {
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
                                                           null_ok = FALSE)
    demcheck::err_multiple_of_n(x = break_max,
                                name = "break_max",
                                n = 5L,
                                null_ok = FALSE)
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    ## get age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## make breaks
    breaks <- make_breaks_integer_lifetab(break_max)
    ## make labels for breaks
    labels <- make_labels_age_group(breaks = breaks,
                                    open_last = TRUE,
                                    include_na = FALSE)
    plot_date_to_age_group(date = date,
                           dob = dob,
                           breaks = breaks,
                           unit = "year",
                           open_last = TRUE,
                           labels = labels,
                           show_months = show_months)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_group_births'
#'
#' Create plot illustrating the inputs and outputs
#' of function \code{\link{date_to_age_group_births}}.
#' 
#' This function is for learning about the
#' labelling conventions used in the \strong{dem} packages,
#' and about their implementation in package
#' \strong{demprep}. It would not normally be used
#' during actual data analysis.
#'
#' @inheritParams plot_date_to_age_group_year
#' @param date Dates when births being measured occur.
#' @param dob Dates of birth of monthers.
#' @param break_min An integer or \code{NULL}.
#' Defaults to 15.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 50.
#' @param width The width in years of the age intervals.
#' A positive integer defaulting to 5.
#' @param recode_up If \code{TRUE}, births to women
#' aged less than \code{break_min} are treated as occurring to
#' women in the lowest repoductive age group.
#' @param recode_down If \code{TRUE}, births to women
#' aged \code{break_max} or more are treated as
#' occurring to women in the highest reproductive
#' age group.
#'
#' @examples
#' plot_date_to_age_group_births(date = c("2024-03-27", "2022-11-09"),
#'                               dob = c("2001-03-21", "2000-07-13"))
#' plot_date_to_age_group_births(date = c("2024-03-27", "2022-11-09"),
#'                               dob = c("2001-03-21", "2000-07-13"),
#'                               width = 10,
#'                               break_min = 20)
#'
#' ## allow youngest and oldest age groups to be
#' ## set by the data
#' plot_date_to_age_group_births(date = c("2052-01-02", "2019-09-22", "2022-10-08"),
#'                               dob = c("2000-01-01", "2001-03-17", "2010-07-05"),
#'                               break_min = NULL,
#'                               break_max = NULL)
#'
#' ## recode ages outside the expected range
#' plot_date_to_age_group_births(date = c("2052-01-02", "2019-09-22", "2022-10-08"),
#'                               dob = c("2000-01-01", "2001-03-17", "2010-07-05"),
#'                               recode_up = TRUE,
#'                               recode_down = TRUE)
#' @export
plot_date_to_age_group_births <- function(date, dob,
                                          break_min = 15,
                                          break_max = 50,
                                          width = 5,
                                          recode_up = FALSE,
                                          recode_down = FALSE,
                                          show_months = FALSE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    demcheck::err_has_non_na(x = date,
                             name = "date")
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
        if ((break_max - break_min) %% width != 0L)
            stop(gettextf("difference between '%s' [%d] and '%s' [%d] not divisible by '%s' [%d]",
                          "break_max", break_max, "break_min", break_min, "width", width))
    }
    demcheck::err_is_logical_flag(x = recode_up,
                                  name = "recode_up")
    demcheck::err_is_logical_flag(x = recode_down,
                                  name = "recode_down")
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
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
                              break_min))
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
                              break_max))
            }
        }
    }
    ## make breaks
    breaks <- make_breaks_integer_births(age = age_years,
                                         width = width,
                                         break_min = break_min,
                                         break_max = break_max)
    ## make labels for breaks
    labels <- make_labels_age_group(breaks = breaks,
                                    open_last = FALSE)
    ## make plot
    plot_date_to_age_group(date = date,
                           dob = dob,
                           breaks = breaks,
                           unit = "year",
                           open_last = FALSE,
                           labels = labels,
                           show_months = show_months)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_group_custom'
#'
#' Create plot illustrating the inputs and outputs
#' of function \code{\link{date_to_age_group_custom}}.
#' 
#' This function is for learning about the
#' labelling conventions used in the \strong{dem} packages,
#' and about their implementation in package
#' \strong{demprep}. It would not normally be used
#' during actual data analysis.
#'
#' @inheritParams plot_date_to_age_group_year
#' @param breaks A vector of strictly increasing integer values.
#'
#' @examples
#' plot_date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#'                               dob = c("2001-03-21", "2000-07-13"),
#'                               breaks = c(0, 15, 60))
#' plot_date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#'                               dob = c("2001-03-21", "2000-07-13"),
#'                               breaks = c(15, 40, 65))
#'
#' ## alternative specifications for oldest age group
#' date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#'                          dob = c("2001-03-21", "2000-07-13"),
#'                          breaks = c(15, 65, 100))
#' date_to_age_group_custom(date = c("2024-03-27", "2022-11-09"),
#'                          dob = c("2001-03-21", "2000-07-13"),
#'                          breaks = c(15, 65, 100),
#'                          open_last = FALSE)
#' @export
plot_date_to_age_group_custom <- function(date, dob,
                                          breaks = NULL,
                                          open_last = TRUE,
                                          show_months = FALSE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    demcheck::err_has_non_na(x = date,
                             name = "date")
    breaks <- demcheck::err_tdy_breaks_integer_age(breaks = breaks,
                                                   open_last = open_last)
    n_break <- length(breaks)
    demcheck::err_has_non_na(x = breaks,
                             name = "breaks")
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
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
                      breaks[[1L]]))
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
                          breaks[[n_break]]))
        }
    }
    ## make labels for breaks
    labels <- make_labels_age_group(breaks = breaks,
                                    open_last = open_last,
                                    include_na = FALSE)
    ## make plot
    plot_date_to_age_group(date = date,
                           dob = dob,
                           breaks = breaks,
                           unit = "year",
                           open_last = open_last,
                           labels = labels,
                           show_months = show_months)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_group_quarter'
#'
#' Create plot illustrating the inputs and outputs
#' of function \code{\link{date_to_age_group_quarter}}.
#' 
#' This function is for learning about the
#' labelling conventions used in the \strong{dem} packages,
#' and about their implementation in package
#' \strong{demprep}. It would not normally be used
#' during actual data analysis.
#'
#' @inheritParams plot_date_to_age_group_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 400.
#'
#' @examples
#' plot_date_to_age_group_quarter(date = c("2004-03-27", "2002-11-09"),
#'                                dob = c("2001-03-21", "2000-07-13"))
#'
#' ## alternative specifications for oldest age group
#' plot_date_to_age_group_quarter(date = "2019-09-22",
#'                                dob = "1910-01-01")
#' plot_date_to_age_group_quarter(date = "2019-09-22",
#'                                dob = "1910-01-01",
#'                                break_max = 320)
#' plot_date_to_age_group_quarter(date = "2019-09-22",
#'                                dob = "1910-01-01",
#'                                break_max = NULL)
#' plot_date_to_age_group_quarter(date = "2019-09-22",
#'                                dob = "1910-01-01",
#'                                break_max = NULL,
#'                                open_last = FALSE)
#' @export
plot_date_to_age_group_quarter <- function(date,
                                           dob,
                                           break_max = 400,
                                           open_last = TRUE,
                                           show_months = FALSE) {
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
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    ## get age in months and quarters
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    ## if final interval not open, check that all
    ## ages less than 'break_max'
    if (!is.null(break_max) && !open_last)
        demcheck::err_lt_break_max_age(age = age_quarters,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "quarter")
    ## make breaks
    breaks <- make_breaks_integer_month_quarter(age = age_quarters,
                                                break_max = break_max,
                                                open_last = open_last)
    ## make labels for these breaks
    n_break <- length(breaks)
    break_max <- breaks[[n_break]]
    labels <- make_labels_age_group_quarter(break_min = 0L,
                                            break_max = break_max,
                                            open_last = open_last)
    ## make plot
    plot_date_to_age_group(date = date,
                           dob = dob,
                           breaks = breaks,
                           unit = "quarter",
                           open_last = open_last,
                           labels = labels,
                           show_months = show_months)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_group_month'
#'
#' Create plot illustrating the inputs and outputs
#' of function \code{\link{date_to_age_group_month}}.
#' 
#' This function is for learning about the
#' labelling conventions used in the \strong{dem} packages,
#' and about their implementation in package
#' \strong{demprep}. It would not normally be used
#' during actual data analysis.
#'
#' @inheritParams plot_date_to_age_group_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 1200.
#'
#' @examples
#' plot_date_to_age_group_month(date = c("2004-03-27", "2002-11-09"),
#'                              dob = c("2001-03-21", "2000-07-13"))
#'
#' ## alternative specifications for oldest age group
#' plot_date_to_age_group_month(date = "2019-09-22",
#'                              dob = "1910-01-01")
#' plot_date_to_age_group_month(date = "2019-09-22",
#'                              dob = "1910-01-01",
#'                              break_max = 320)
#' plot_date_to_age_group_month(date = "2019-09-22",
#'                              dob = "1910-01-01",
#'                              break_max = NULL)
#' plot_date_to_age_group_month(date = "2019-09-22",
#'                              dob = "1910-01-01",
#'                              break_max = NULL,
#'                              open_last = FALSE)
#' @export
plot_date_to_age_group_month <- function(date,
                                         dob,
                                         break_max = 1200,
                                         open_last = TRUE,
                                         show_months = FALSE) {
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
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    ## get age in months
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    ## if final interval not open, check that all
    ## ages less than 'break_max'
    if (!open_last && !is.null(break_max))
        demcheck::err_lt_break_max_age(age = age_months,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "month")
    ## make breaks
    breaks <- make_breaks_integer_month_quarter(age = age_months,
                                                break_max = break_max,
                                                open_last = open_last)
    ## make labels for these breaks
    n_break <- length(breaks)
    break_max <- breaks[[n_break]]
    labels <- make_labels_age_group_month(break_min = 0L,
                                          break_max = break_max,
                                          open_last = open_last,
                                          include_na = FALSE)
    ## make plot
    plot_date_to_age_group(date = date,
                           dob = dob,
                           unit = "month",
                           breaks = breaks,
                           open_last = open_last,
                           labels = labels,
                           show_months = show_months)
}
