
## NO_TESTS
#' Depict the Lexis triangles created by
#' function 'date_to_triangle_year'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_triangle_year}} works.
#' 
#' \code{plot_date_to_triangle_year} is typically used for
#' instruction or documentation, rather than for
#' actual data analysis.
#'
#' @param date Dates of events.
#' @param dob Dates of birth.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The period starts on
#' the first day of this month.
#' @param show_months Whether to include vertical
#' lines showing boundaries between months.
#' Defaults to \code{FALSE}.
#'
#' @examples
#' plot_date_to_triangle_year(date = c("2024-05-27",
#'                                     "2024-11-09"),
#'                            dob = c("2020-03-13",
#'                                    "2021-08-24"))
#'
#' ## start on 1 July, rather than 1 January
#' plot_date_to_triangle_year(date = c("2024-05-27",
#'                                     "2024-11-09"),
#'                            dob = c("2020-03-13",
#'                                    "2021-08-24"),
#'                            month_start = "July")
#' @keywords internal
#' @export
plot_date_to_triangle_year <- function(date,
                                       dob,
                                       month_start = "Jan",
                                       label_year_start = TRUE,
                                       show_months = FALSE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    ## calculate age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## make time breaks
    dob_date <- sort(unique(c(dob, date)))
    breaks_time <- make_breaks_date_to_date_year(date = dob_date,
                                                 month_start = month_start)
    ## make labels for time breaks
    use_first_break <- identical(month_start, "Jan") || label_year_start
    if (use_first_break)
        breaks_labels <- breaks_time[-length(breaks_time)]
    else
        breaks_labels <- breaks_time[-1L]
    labels_time <- format(breaks_labels, "%Y")
    ## make age breaks
    break_max_age <- max(age_years, na.rm = TRUE) + 1L
    breaks_age <- seq.int(from = 0L,
                          to = break_max_age)
    ## make labels for age breaks
    breaks_age_labels <- breaks_age[-length(breaks_age)]
    labels_age <- as.character(breaks_age_labels)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "year",
                              breaks_time = breaks_time,
                              breaks_age = breaks_age,
                              labels_time = labels_time,
                              labels_age = labels_age,
                              show_months = show_months,
                              show_vert = TRUE,
                              show_diag = TRUE)
}


## NO_TESTS
#' Depict the Lexis triangles created by
#' function 'date_to_triangle_quarter'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_triangle_quarter}} works.
#' 
#' \code{plot_date_to_triangle_quarter} is typically used for
#' instruction or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_triangle_year
#'
#' @examples
#' plot_date_to_triangle_quarter(date = c("2024-03-27",
#'                                        "2022-11-09"),
#'                               dob = c("2020-05-01",
#'                                       "2021-06-22"))
#' @keywords internal
#' @export
plot_date_to_triangle_quarter <- function(date,
                                          dob,
                                          show_months = FALSE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    ## calculate age in months and quarters
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    ## make time breaks
    dob_date <- sort(unique(c(dob, date)))
    breaks_time <- make_breaks_date_to_date_quarter(dob_date)
    ## make labels for time breaks
    breaks_time_labels <- breaks_time[-length(breaks_time)]
    year <- format(breaks_time_labels, "%Y")
    quarter <- quarters(breaks_time_labels)
    labels_time <- paste(year, quarter)
    ## make age breaks
    break_max_age <- max(age_quarters, na.rm = TRUE) + 1L
    breaks_age <- seq.int(from = 0L,
                          to = break_max_age)
    ## make labels for age breaks
    breaks_age_labels <- breaks_age[-length(breaks_age)]
    labels_age <- as.character(breaks_age_labels)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "quarter",
                              breaks_time = breaks_time,
                              breaks_age = breaks_age,
                              labels_time = labels_time,
                              labels_age = labels_age,
                              show_months = show_months,
                              show_vert = TRUE,
                              show_diag = TRUE)
}

## NO_TESTS
#' Depict the Lexis triangles created by
#' function 'date_to_triangle_month'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_triangle_month}} works.
#' 
#' \code{plot_date_to_triangle_month} is typically used for
#' instruction or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_triangle_year
#'
#' @examples
#' plot_date_to_triangle_month(date = c("2024-03-27",
#'                                      "2024-11-09"),
#'                             dob = c("2023-10-05",
#'                                     "2024-03-11"))
#' @keywords internal
#' @export
plot_date_to_triangle_month <- function(date,
                                        dob) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    ## calculate age in months
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    ## make time breaks
    dob_date <- sort(unique(c(dob, date)))
    breaks_time <- make_breaks_date_to_date_month(dob_date)
    ## make labels for time breaks
    breaks_time_labels <- breaks_time[-length(breaks_time)]
    labels_time <- format(breaks_time_labels, "%Y %b")
    ## make age breaks
    break_max_age <- max(age_months, na.rm = TRUE) + 1L
    breaks_age <- seq.int(from = 0L,
                          to = break_max_age)
    ## make labels for age breaks
    breaks_age_labels <- breaks_age[-length(breaks_age)]
    labels_age <- as.character(breaks_age_labels)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "month",
                              breaks_time = breaks_time,
                              breaks_age = breaks_age,
                              labels_time = labels_time,
                              labels_age = labels_age,
                              show_months = FALSE,
                              show_vert = TRUE,
                              show_diag = TRUE)
}

