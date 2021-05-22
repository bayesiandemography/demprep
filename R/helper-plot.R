
## Functions to help with plotting that end users do not use directly.


## HAS_TESTS
## Return coordinates, measured in days,
## for lines representing lifelines. Each
## month on the age axis is 31 days, regardless
## of the length of a month on the time axis.
## To achieve agreement, lifelines are shifted
## upwards by 0-3 days at the end of each month.
## 'date1' and 'dob1' both have length 1
coord_lifeline <- function(date1, dob1) {
    first_boundary <- rollforward_month(dob1)
    boundaries <- seq.Date(from = first_boundary,
                           to = date1,
                           by = "month")
    x <- c(dob1, rep(boundaries, each = 2L), date1)
    diff_y <- as.integer(diff(x))
    first_start <- rollback_month(dob1)
    starts <- seq.Date(from = first_start,
                       by = "month",
                       length.out = length(boundaries))
    n_day_month <- n_day_month(starts)
    shifts <- 31L - n_day_month
    i_shifts <- seq.int(from = 2L,
                        by = 2L,
                        length.out = length(shifts))
    diff_y[i_shifts] <- shifts
    y <- c(0L, cumsum(diff_y))
    n <- length(x)
    list(x0 = x[-n],
         y0 = y[-n],
         x1 = x[-1L],
         y1 = y[-1L])
}


## Testing plotting funtions is hard, but these have some
## partial tests via the examples and vignettes.

## NO_TESTS
plot_date_to_age_triangle <- function(date,
                                      dob,
                                      unit,
                                      breaks_time,
                                      breaks_age,
                                      labels_time,
                                      labels_age,
                                      show_months,
                                      show_vert,
                                      show_diag,
                                      cex = 0.8) {
    old_par <- graphics::par(mar = c(6, 3, 2, 1),
                             mgp = c(0, 0, 0),
                             cex = cex)
    n_date <- length(date)
    days_per_unit <- switch(unit,
                            month = 31L,
                            quarter = 31L * 3L,
                            year = 31L * 12L,
                            stop("invalid unit"))
    n_br_time <- length(breaks_time)
    n_br_age <- length(breaks_age)
    diff_br_age <- diff(breaks_age)
    date_min <- min(dob, na.rm = TRUE)
    date_min <- rollback_month(date_min)
    if (!is.null(breaks_time))
        date_min <- min(date_min, breaks_time)
    date_max <- max(date, na.rm = TRUE)
    date_max <- rollforward_month(date_max)
    if (!is.null(breaks_time))
        date_max <- max(date_max, breaks_time)
    width_date <- date_max - date_min
    age_approx <- as.integer(date - dob) / days_per_unit
    age_max <- max(breaks_age, 1.05 * age_approx)
    x_plot <- c(date_min - 0.15 * width_date, date_max)
    y_plot <- c(0, age_max)
    ## empty plotting frame
    plot(x = x_plot,
         y = y_plot,
         pch = NA,
         axes = FALSE,
         ylab = "",
         xlab = "")
    ## vertical lines to show boundaries between months (optional)
    if (show_months) {
        boundaries_months <- seq(from = date_min,
                                 to = date_max,
                                 by = "month")
        graphics::segments(x0 = boundaries_months,
                           y0 = 0,
                           x1 = boundaries_months,
                           y1 = breaks_age[[n_br_age]],
                           col = "grey")
        graphics::mtext(text = dob,
                        side = 1,
                        line = -0.5,
                        at = boundaries_months,
                        cex = 0.6,
                        las = 3,
                        col = "grey")
    }
    ## vertical lines and labels to show time breaks (optional)
    if (show_vert) {
        ## lines
        graphics::segments(x0 = breaks_time,
                           y0 = 0,
                           x1 = breaks_time,
                           y1 = age_max,
                           lwd = 0.5,
                           col = "cornflowerblue")
        ## labels for boundaries
        graphics::mtext(text = breaks_time,
                        side = 1,
                        line = -0.5,
                        at = breaks_time,
                        cex = 0.5,
                        las = 3,
                        col = "cornflowerblue")
        ## labels for periods
        labels_time <- sprintf('"%s"', labels_time)
        graphics::mtext(text = labels_time,
                        side = 1,
                        line = 3,
                        at = breaks_time[-n_br_time] + 0.5 * diff(breaks_time),
                        cex = 0.7,
                        las = 1,
                        col = "black")
    }
    ## diagonal lines to show Lexis triangles (optional)
    if (show_diag) {
        ## diagonal lines starting at horizontal axis
        n_age <- n_br_age
        date_diag_start <- breaks_time[-n_br_time]
        i_date_diag_end <- seq.int(from = n_age,
                                   along.with = date_diag_start)
        i_date_diag_end <- pmin(i_date_diag_end, n_br_time)
        date_diag_end <- breaks_time[i_date_diag_end]
        break_age_min <- min(breaks_age)
        for (i in seq_along(date_diag_start)) {
            date1 <- date_diag_end[[i]]
            dob1 <- date_diag_start[[i]]
            if (date1 > dob1) {
                coord <- coord_lifeline(date1 = date1,
                                        dob1 = dob1)
                x0 <- coord$x0
                y0 <- coord$y0 / days_per_unit
                x1 <- coord$x1
                y1 <- coord$y1 / days_per_unit
                graphics::segments(x0 = x0,
                                   y0 = y0 + break_age_min, # for births
                                   x1 = x1,
                                   y1 = y1 + break_age_min, # for births
                                   lwd = 0.5,
                                   col = "cornflowerblue")
            }
        }
        ## diagonal lines starting at left
        date_diag_start <- rep(breaks_time[[1L]], times = n_br_age - 2L)
        i_end <- pmin((n_br_age - 1L) : 2L, n_br_time)
        date_diag_end <- breaks_time[i_end]
        for (i in seq_along(date_diag_start)) {
            date1 <- date_diag_end[[i]]
            dob1 <- date_diag_start[[i]]
            coord <- coord_lifeline(date1 = date1,
                                    dob1 = dob1)
            x0 <- coord$x0
            y0 <- coord$y0 / days_per_unit + breaks_age[[i + 1L]]
            x1 <- coord$x1
            y1 <- coord$y1 / days_per_unit + breaks_age[[i + 1L]]
            graphics::segments(x0 = x0,
                               y0 = y0,
                               x1 = x1,
                               y1 = y1,
                               lwd = 0.5,
                               col = "cornflowerblue")
        }
    }
    ## horizontal lines to show boundaries between age groups
    y_horiz <- unique(c(0, breaks_age)) # needed for date_to_age_births
    graphics::segments(x0 = rep(date_min, times = n_br_age),
                       y0 = y_horiz,
                       x1 = rep(date_max, times = n_br_age),
                       y1 = y_horiz,
                       lwd = 0.5,
                       lty = "solid",
                       col = "cornflowerblue")
    ## labels for boundaries between age groups
    graphics::text(x = date_min - 0.02 * width_date,
                   y = breaks_age,
                   labels = breaks_age,
                   cex = 0.7,
                   col = "cornflowerblue")
    ## labels for age groups
    labels_age <- sprintf('"%s"', labels_age)
    y_labels_age <- breaks_age[-n_br_age] + 0.5 * diff_br_age
    graphics::text(x = date_min - 0.04 * width_date,
                   y = y_labels_age,
                   labels = labels_age,
                   pos = 2,
                   cex = 0.9,
                   col = "black")
    ## points for 'dob'
    graphics::points(x = dob,
                     y = rep(0, times = n_date),
                     pch = 19,
                     cex = 0.8)    
    ## labels for 'dob'
    graphics::mtext(text = dob,
                    side = 1,
                    line = -0.5,
                    at = dob,
                    cex = 0.6,
                    las = 3,
                    col = "black")
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
                           y1 = y1)
        graphics::points(x = date[[i]],
                         y = y1[[length(y1)]],
                         pch = 19,
                         cex = 0.8)
    }        
    ## labels for 'date'
    graphics::mtext(text = date,
                    side = 1,
                    line = -0.5,
                    at = date,
                    cex = 0.6,
                    las = 3,
                    col = "black")
    ## xlab
    graphics::mtext(text = "Time",
                    side = 1,
                    line = 4.5,
                    cex = 0.7,
                    col = "grey35")
    graphics::par(old_par)
    ## ylab
    graphics::mtext(text = "Age",
                    side = 2,
                    line = 1.5,
                    las = 1,
                    cex = 0.7,
                    col = "grey35")
    graphics::par(old_par)
    invisible(NULL)
}



## NO_TESTS
plot_date_to_cohort_period <- function(date, breaks, open_first, labels, cex = 0.8) {
    old_par <- graphics::par(mar = c(1, 0, 0, 0),
                             mgp = c(0, 0, 0),
                             cex = cex)
    n_date <- length(date)
    n_br <- length(breaks)
    diff_br <- diff(breaks)
    diff_br_all <- breaks[[n_br]] - breaks[[1L]]
    x_plot_first <- breaks[[1L]] - 0.03 * diff_br_all
    if (open_first) {
        x_plot_first <- min(x_plot_first,
                            breaks[[1L]] - diff_br[[1L]])
        x_plot_first <- min(x_plot_first,
                            min(date, na.rm = TRUE) - 0.2 * diff_br[[1L]])
    }        
    x_plot_last <- breaks[[n_br]] + 0.03 * diff_br_all
    x_plot <- c(x_plot_first, x_plot_last)
    y_plot <- rep(0, 2L)
    ## empty plotting frame
    plot(x = x_plot,
         y = y_plot,
         pch = NA,
         axes = FALSE,
         ylab = "",
         xlab = "")
    ## x-axis and ticks
    graphics::lines(x = breaks,
                    y = rep(0, times = n_br),
                    col = "cornflowerblue")
    if (open_first) {
        x_first <- min(date, na.rm = TRUE) - 0.2 * diff_br[[1L]]
        x_first <- min(x_first, breaks[[1L]] - diff_br[[1L]])
        graphics::lines(x = c(x_first, breaks[[1L]]),
                        y = c(0, 0),
                        col = "cornflowerblue")
    }
    graphics::segments(x0 = breaks,
                       y0 = -0.1,
                       x1 = breaks,
                       y1 = 0.1,
                       col = "cornflowerblue")
    ## labels for breaks
    graphics::text(x = breaks,
                   y = -0.15,
                   labels = breaks,
                   cex = 0.7,
                   adj = 1,
                   srt = 90,
                   col = "cornflowerblue")
    ## labels for cohorts
    x_lab <- breaks[-n_br] + 0.5 * diff_br
    if (open_first)
        x_lab <- c(breaks[[1L]] - 0.5 * diff_br[[1L]],
                   x_lab)
    graphics::text(x = x_lab,
                   y = 0.3,
                   labels = sprintf('"%s"', labels))
    ## dates
    graphics::points(x = date,
                     y = rep(0, times = n_date),
                     pch = 19,
                     col = "black",
                     cex = 0.8)
    ## labels for 'date'
    graphics::text(x = date,
                   y = -0.05,
                   labels = date,
                   cex = 0.9,
                   adj = 1,
                   srt = 90,
                   col = "black")
    ## xlab
    graphics::mtext(text = "Time",
                    side = 1,
                    line = 0,
                    cex = 0.7,
                    col = "grey35")
    graphics::par(old_par)
    invisible(NULL)
}


## NO_TESTS
plot_date_to_cohort_period_year <- function(date,
                                     month_start = "Jan",
                                     label_year_start = TRUE) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_year(date = date,
                                            month_start = month_start)
    ## create labels
    use_first_break <- identical(month_start, "Jan") || label_year_start
    if (use_first_break)
        breaks_labels <- breaks[-length(breaks)]
    else
        breaks_labels <- breaks[-1L]
    labels <- format(breaks_labels, "%Y")
    ## make plot
    plot_date_to_cohort_period(date = date,
                               breaks = breaks,
                               open_first = FALSE,
                               labels = labels)
}

## NO_TESTS
plot_date_to_cohort_period_quarter <- function(date) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_quarter(date)
    ## make labels for these breaks
    breaks_labels <- breaks[-length(breaks)]
    year <- format(breaks_labels, "%Y")
    quarters <- quarters(breaks_labels)
    labels <- paste(year, quarters)
    ## make plot
    plot_date_to_cohort_period(date = date,
                               breaks = breaks,
                               open_first = FALSE,
                               labels = labels)
}

## NO_TESTS
plot_date_to_cohort_period_month <- function(date) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_month(date)
    ## make labels for these breaks
    breaks_labels <- breaks[-length(breaks)]
    labels <- format(breaks_labels, "%Y %b")
    ## make plot
    plot_date_to_cohort_period(date = date,
                               breaks = breaks,
                               open_first = FALSE,
                               labels = labels)
}
