
## NO_TESTS
plot_date_to_period <- function(date, breaks, labels) {
    old_par <- graphics::par(mar = rep(0, 4))
    n_date <- length(date)
    n_br <- length(breaks)
    diff_br <- diff(breaks)
    x_plot <- c(breaks[[1L]] - 0.5 * diff_br[[1L]],
                breaks[[n_br]] + 0.5 * diff_br[[n_br - 1L]])
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
                    y = rep(0, times = n_br))
    graphics::segments(x0 = breaks,
                       y0 = -0.1,
                       x1 = breaks,
                       y1 = 0.1)
    ## values for breaks
    graphics::text(x = breaks,
                   y = -0.2,
                   labels = breaks,
                   cex = 0.8)
    ## labels for periods
    graphics::text(x = breaks[-n_br] + 0.5 * diff_br,
                   y = 0.25,
                   labels = sprintf('"%s"', labels))
    ## dates
    graphics::points(x = date,
                     y = rep(0, times = n_date),
                     pch = 19, 
                     cex = 0.7)
    graphics::par(old_par)
    invisible(NULL)
}


## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_period_year'
#'
#' @param date Dates of events or measurements.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The period starts on
#' the first day of this month.
#' @param label_year_start Whether to label a period
#' by the calendar year at the beginning of the period
#' or the calendar year at the end. Not needed for periods
#' that start on 1 January. Defaults to \code{FALSE}.
#'
#' @seealso \code{\link{date_to_period_year}}
#'
#' @examples
#' plot_date_to_period_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## July to June
#' plot_date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                          month_start = "Jul")
#'
#' ## July to June, using the calendar year at
#' ## the end for the label
#' plot_date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                          month_start = "Jul",
#'                          label_year_start = FALSE)
#' @export
plot_date_to_period_year <- function(date,
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
    breaks <- make_breaks_date_year(date = date,
                                    month_start = month_start,
                                    width = 1L,
                                    origin = NULL,
                                    break_min = NULL)
    ## create labels
    labels <- make_labels_period(breaks = breaks,
                                 label_year_start = label_year_start,
                                 include_na = FALSE)
    ## make plot
    plot_date_to_period(date = date,
                        breaks = breaks,
                        labels = labels)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_period_multi'
#'
#' @inheritParams plot_date_to_period_year
#' @param width The length, in whole years, of the periods.
#' Defaults to 5.
#' @param origin An integer. Defaults to 2000.
#'#'
#' @seealso \code{\link{date_to_period_multi}}
#'
#' @examples
#' plot_date_to_period_multi(date = c("2024-03-27",
#'                                    "2018-11-09",
#'                                    "2021-03-02"))
#'
#' ## width is 10
#' plot_date_to_period_multi(date = c("2024-03-27",
#'                                    "2018-11-09",
#'                                    "2021-03-02"),
#'                           width = 5)
#'
#' ## origin is 2001
#' plot_date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                           origin = 2001)
#'
#' ## July to June
#' plot_date_to_period_multi(date = c("2024-03-27",
#'                               "2018-11-09",
#'                               "2021-03-02"),
#'                           origin = 2001,
#'                           month_start = "Jul")
#' @export
plot_date_to_period_multi <- function(date,
                                      width = 5,
                                      origin = 2000,
                                      month_start = "Jan") {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    ## create sequence of breaks
    breaks <- make_breaks_date_year(date = date,
                                    month_start = month_start,
                                    width = width,
                                    origin = origin,
                                    break_min = NULL)
    ## make labels for these breaks
    labels <- make_labels_cohort(breaks = breaks,
                                 open_first = FALSE,
                                 label_year_start = NULL,
                                 include_na = FALSE)
    ## make plot
    plot_date_to_period(date = date,
                        breaks = breaks,
                        labels = labels)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_period_custom'
#'
#' @inheritParams plot_date_to_period_year
#' @param breaks Dates defining starts and ends of periods.
#'
#' @seealso \code{\link{date_to_period_custom}}
#'
#' @examples
#' ## periods start on 1 January
#' plot_date_to_period_custom(date = c("2024-03-27",
#'                                     "2018-11-09",
#'                                     "2021-03-02"),
#'                            breaks = c("2000-01-01",
#'                                       "2019-01-01",
#'                                       "2026-01-01"))
#'
#' ## periods start on 1 March
#' plot_date_to_period_custom(date = c("2024-03-27",
#'                                     "2018-11-09",
#'                                     "2021-03-02"),
#'                            breaks = c("2000-03-01",
#'                                       "2019-03-01",
#'                                       "2026-03-01"))
#' @export
plot_date_to_period_custom <- function(date, breaks) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    demcheck::err_positive_length(x = breaks,
                                  name = "breaks")
    breaks <- demcheck::err_tdy_breaks_date_period(breaks = breaks)
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    demcheck::err_ge_break_min_date(date = date,
                                    break_min = break_min)
    demcheck::err_lt_break_max_date(date = date,
                                    break_max = break_max)
    ## make labels for breaks
    labels <- make_labels_period(breaks = breaks,
                                 label_year_start = NULL,
                                 include_na = FALSE)
    ## make plot
    plot_date_to_period(date = date,
                        breaks = breaks,
                        labels = labels)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_period_quarter'
#'
#' @inheritParams plot_date_to_period_year
#'
#' @seealso \code{\link{date_to_period_quarter}}
#' 
#' @examples
#' plot_date_to_period_quarter(date = c("2024-03-27",
#'                                      "2020-01-03",
#'                                      "2022-11-09"))
#' @export
plot_date_to_period_quarter <- function(date) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    ## create sequence of breaks
    breaks <- make_breaks_date_quarter(date = date,
                                       break_min = NULL)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_period_quarter(break_min = break_min,
                                         break_max = break_max,
                                         include_na = FALSE)
    ## make plot
    plot_date_to_period(date = date,
                        breaks = breaks,
                        labels = labels)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_period_month'
#'
#' @inheritParams plot_date_to_period_year
#'
#' @seealso \code{\link{date_to_period_month}}
#' 
#' @examples
#' plot_date_to_period_month(date = c("2024-03-27",
#'                                    "2020-01-03",
#'                                    "2022-11-09"))
#' @export
plot_date_to_period_month <- function(date) {
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    demcheck::err_has_non_na(x = date,
                             name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    ## create sequence of breaks
    breaks <- make_breaks_date_month(date = date,
                                     break_min = NULL)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_period_month(break_min = break_min,
                                       break_max = break_max,
                                       include_na = FALSE)
    ## make plot
    plot_date_to_period(date = date,
                        breaks = breaks,
                        labels = labels)
}
