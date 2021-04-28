
## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_year'
#'
#' Create a plot illustrating how function
#' \code{\link{date_to_age_year}} works.
#' 
#' \code{plot_date_to_age_year} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @param date Dates of events or measurements.
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
#' @param show_months Whether to include vertical
#' lines showing boundaries between months.
#' Defaults to \code{FALSE}.
#'
#' @examples
#' plot_date_to_age_year(date = c("2002-11-09",
#'                                "2004-04-27"),
#'                       dob = c("2000-07-13",
#'                               "2001-03-21"),
#'                       break_max = 2)
#' plot_date_to_age_year(date = c("2002-11-09",
#'                                "2004-04-27"),
#'                       dob = c("2000-07-13",
#'                               "2001-03-21"),
#'                       break_max = 4,
#'                       open_last = FALSE)
#' @keywords internal
#' @export
plot_date_to_age_year <- function(date,
                                  dob,
                                  break_min = 0,
                                  break_max = 100,
                                  open_last = TRUE,
                                  show_months = FALSE) {
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
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
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
    labels <- make_labels_age(breaks = breaks,
                              open_last = open_last,
                              include_na = FALSE)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "year",
                              breaks_time = NULL,
                              breaks_age = breaks,
                              open_last = open_last,
                              labels_time = NULL,
                              labels_age = labels,
                              show_months = show_months,
                              show_vert = FALSE,
                              show_diag = FALSE)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_quarter'
#'
#' Create a plot illustrating how function
#' \code{\link{date_to_age_quarter}} works.
#' 
#' \code{plot_date_to_age_quarter} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_age_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 400.
#'
#' @examples
#' plot_date_to_age_quarter(date = c("2004-03-27",
#'                                   "2002-11-09"),
#'                          dob = c("2001-03-21",
#'                                  "2000-07-13"),
#'                          break_max = 12)
#'
#' ## alternative specifications for oldest age group
#' plot_date_to_age_quarter(date = "2019-09-22",
#'                          dob = "2018-01-01",
#'                          break_max = 4)
#' plot_date_to_age_quarter(date = "2019-09-22",
#'                          dob = "2018-01-01",
#'                          break_max = NULL)
#' plot_date_to_age_quarter(date = "2019-09-22",
#'                          dob = "2018-01-01",
#'                          break_max = NULL,
#'                          open_last = FALSE)
#' @keywords internal
#' @export
plot_date_to_age_quarter <- function(date,
                                     dob,
                                     break_min = 0,
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
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
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
    labels <- make_labels_age_quarter(break_min = break_min,
                                      break_max = break_max,
                                      open_last = open_last)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "quarter",
                              breaks_time = NULL,
                              breaks_age = breaks,
                              open_last = open_last,
                              labels_time = NULL,
                              labels_age = labels,
                              show_months = show_months,
                              show_vert = FALSE,
                              show_diag = FALSE)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_month'
#'
#' Create a plot illustrating how function
#' \code{\link{date_to_age_month}} works.
#' 
#' \code{plot_date_to_age_month} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_age_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 1200.
#'
#' @examples
#' plot_date_to_age_month(date = c("2004-03-27",
#'                                 "2004-11-09"),
#'                        dob = c("2003-03-21",
#'                                "2003-07-13"),
#'                        break_max = 24)
#'
#' ## alternative specifications for oldest age group
#' plot_date_to_age_month(date = "2019-09-22",
#'                        dob = "2019-01-01",
#'                        break_max = 12)
#' plot_date_to_age_month(date = "2019-09-22",
#'                        dob = "2019-01-01",
#'                        break_max = NULL)
#' plot_date_to_age_month(date = "2019-09-22",
#'                        dob = "2019-01-01",
#'                        break_max = NULL,
#'                        open_last = FALSE)
#' @keywords internal
#' @export
plot_date_to_age_month <- function(date,
                                   dob,
                                   break_min = 0,
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
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
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
    labels <- make_labels_age_month(break_min = break_min,
                                    break_max = break_max,
                                    open_last = open_last,
                                    include_na = FALSE)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "month",
                              breaks_time = NULL,
                              breaks_age = breaks,
                              open_last = open_last,
                              labels_time = NULL,
                              labels_age = labels,
                              show_months = show_months,
                              show_vert = FALSE,
                              show_diag = FALSE)
}
