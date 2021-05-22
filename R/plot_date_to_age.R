
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
#' @param show_months Whether to include vertical
#' lines showing boundaries between months.
#' Defaults to \code{FALSE}.
#'
#' @examples
#' plot_date_to_age_year(date = c("2002-11-09",
#'                                "2004-04-27"),
#'                       dob = c("2000-07-13",
#'                               "2001-03-21"))
#' plot_date_to_age_year(date = c("2002-11-09",
#'                                "2004-04-27"),
#'                       dob = c("2000-07-13",
#'                               "2001-03-21"),
#'                       show_months = TRUE)
#' @keywords internal
#' @export
plot_date_to_age_year <- function(date,
                                  dob,
                                  show_months = FALSE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    break_max <- max(age_years, na.rm = TRUE) + 1L
    breaks <- seq.int(from = 0L,
                      to = break_max)
    breaks_labels <- breaks[-length(breaks)]
    labels <- as.character(breaks_labels)
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "year",
                              breaks_time = NULL,
                              breaks_age = breaks,
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
#'
#' @examples
#' plot_date_to_age_quarter(date = c("2004-03-27",
#'                                   "2002-11-09"),
#'                          dob = c("2001-03-21",
#'                                  "2000-07-13"))
#' @keywords internal
#' @export
plot_date_to_age_quarter <- function(date,
                                     dob,
                                     show_months = FALSE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    demcheck::err_has_non_na(x = date,
                             name = "date")
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    break_max <- max(age_quarters, na.rm = TRUE) + 1L
    breaks <- seq.int(from = 0L,
                      to = break_max)
    breaks_labels <- breaks[-length(breaks)]
    labels <- as.character(breaks_labels)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "quarter",
                              breaks_time = NULL,
                              breaks_age = breaks,
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
#'
#' @examples
#' plot_date_to_age_month(date = c("2004-03-27",
#'                                 "2004-11-09"),
#'                        dob = c("2003-03-21",
#'                                "2003-07-13"))
#' @keywords internal
#' @export
plot_date_to_age_month <- function(date,
                                   dob) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    demcheck::err_has_non_na(x = date,
                             name = "date")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    break_max <- max(age_months, na.rm = TRUE) + 1L
    breaks <- seq.int(from = 0L,
                      to = break_max)
    breaks_labels <- breaks[-length(breaks)]
    labels <- as.character(breaks_labels)
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "month",
                              breaks_time = NULL,
                              breaks_age = breaks,
                              labels_time = NULL,
                              labels_age = labels,
                              show_months = FALSE,
                              show_vert = FALSE,
                              show_diag = FALSE)
}
