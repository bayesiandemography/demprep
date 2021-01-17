
## NO_TESTS
#' Depict the Lexis triangles created by
#' function 'date_to_triangle_year'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_triangle_year}} works.
#' \code{plot_date_to_triangle_year} is designed for
#' instruction or documentation, rather than for
#' actual data analysis.
#'
#' @param date Dates of events.
#' @param dob Dates of birth.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 100.
#' @param open_last Whether the final age group
#' has no upper limit. Defaults to \code{TRUE}.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The period starts on
#' the first day of this month.
#' @param label_year_start Whether to label a period
#' by the calendar year at the beginning of the period
#' or the calendar year at the end. Not needed for periods
#' that start on 1 January. Defaults to \code{FALSE}.
#' @param show_months Whether to include vertical
#' lines showing boundaries between months.
#' Defaults to \code{FALSE}.
#'
#' @examples
#' plot_date_to_triangle_year(date = c("2024-03-27",
#'                                     "2022-11-09"),
#'                            dob = "2020-01-01")
#'
#' ## July to June
#' plot_date_to_triangle_year(date = c("2024-03-27",
#'                                     "2022-11-09"),
#'                            dob = "2020-01-01",
#'                            month_start = "Jul")
#' @export
plot_date_to_triangle_year <- function(date,
                                       dob,
                                       break_max = 100,
                                       open_last = TRUE,
                                       month_start = "Jan",
                                       label_year_start = TRUE,
                                       show_months = FALSE) {
    plot_date_to_triangle_multi(date = date,
                                dob = dob,
                                width = 1L,
                                break_max = break_max,
                                open_last = open_last,
                                origin = 2000L,
                                month_start = month_start,
                                label_year_start = label_year_start,
                                show_months = FALSE)
}


## NO_TESTS
#' Depict the Lexis triangles created by
#' function 'date_to_triangle_multi'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_triangle_multi}} works.
#' \code{plot_date_to_triangle_multi} is designed for
#' instruction or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_triangle_year
#' @param width The width in years of the periods and
#' age groups. A positive integer defaulting to 5.
#' @param origin An integer. Defaults to 2000. 
#'
#' @examples
#' plot_date_to_triangle_multi(date = c("2027-03-27",
#'                                      "2022-11-09"),
#'                             dob = "2010-05-12")
#'
#' ## width is 10
#' plot_date_to_triangle_multi(date = c("2027-03-27",
#'                                      "2022-11-09"),
#'                             dob = "2010-05-12",
#'                             width = 10)
#'
#'
#' ## period starts on 1 July rather than 1 January
#' plot_date_to_triangle_multi(date = c("2027-03-27",
#'                                      "2022-11-09"),
#'                             dob = "2010-05-12",
#'                             month_start = "Jul")
#'
#' ## open age group starts at 10 years
#' plot_date_to_triangle_multi(date = c("2027-03-27",
#'                                      "2022-11-09"),
#'                             dob = "2003-05-12",
#'                             break_max = 10)
#' @export
plot_date_to_triangle_multi <- function(date,
                                        dob,
                                        width = 5,
                                        break_max = 100,
                                        open_last = TRUE,
                                        origin = 2000,
                                        month_start = "Jan",
                                        label_year_start = TRUE,
                                        show_months = FALSE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    label_year_start <- demcheck::err_is_logical_flag(x = label_year_start,
                                                      name = "label_year_start")
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    ## calculate age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## if has upper limit, check that all
    ## ages less than limit
    if (!is.null(break_max) && !open_last)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    ## make time breaks
    dob_date <- sort(unique(c(dob, date)))
    breaks_time <- make_breaks_date_year(date = dob_date,
                                         month_start = month_start,
                                         width = width,
                                         origin = origin,
                                         break_min = NULL)
    ## make labels for time breaks
    labels_time <- make_labels_period(breaks = breaks_time,
                                      label_year_start = label_year_start,
                                      include_na = FALSE)
    ## make age breaks
    breaks_age <- make_breaks_integer_year(age = age_years,
                                           width = width,
                                           break_max = break_max,
                                           open_last = open_last)
    ## make labels for age breaks
    labels_age <- make_labels_age(breaks = breaks_age,
                                  open_last = open_last)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "year",
                              breaks_time = breaks_time,
                              breaks_age = breaks_age,
                              open_last = open_last,
                              labels_time = labels_time,
                              labels_age = labels_age,
                              show_months = show_months,
                              show_vert = TRUE,
                              show_diag = TRUE)
}

## NO_TESTS
#' Depict the Lexis triangles created by
#' function 'date_to_triangle_births'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_triangle_births}} works.
#' \code{plot_date_to_triangle_births} is designed for
#' instruction or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_triangle_year
#' @param date Dates when births being measured occur.
#' @param dob Dates of birth of parents.
#' @param width The width in years of the periods and
#' age groups. A positive integer defaulting to 5.
#' @param break_min An integer or \code{NULL}.
#' Defaults to 15.
#' @param break_max An integer or \code{NULL}.
#' Defaults to 50.
#' @param recode_up If \code{TRUE}, births to people
#' aged less than \code{break_min} are treated as occurring to
#' people in the lowest repoductive age group.
#' @param recode_down If \code{TRUE}, births to people
#' aged \code{break_max} or more are treated as
#' occurring to people in the highest reproductive
#' age group.
#' @param origin An integer. Defaults to 2000. 
#'
#' @examples
#' plot_date_to_triangle_births(date = c("2024-03-27", "2022-11-09"),
#'                              dob = c("2001-03-21", "2000-07-13"))
#'
#' ## alternative values for 'width'
#' plot_date_to_triangle_births(date = c("2024-03-27", "2022-11-09"),
#'                              dob = c("2001-03-21", "2000-07-13"),
#'                              width = 10,
#'                              break_min = 20)
#' plot_date_to_triangle_births(date = c("2024-03-27", "2022-11-09"),
#'                              dob = c("2001-03-21", "2000-07-13"),
#'                              width = 1)
#' @export
plot_date_to_triangle_births <- function(date,
                                         dob,
                                         width = 5,
                                         break_min = 15,
                                         break_max = 50,
                                         recode_up = FALSE,
                                         recode_down = FALSE,
                                         origin = 2000,
                                         month_start = "Jan",
                                         label_year_start = TRUE,
                                         show_months = FALSE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    break_min <- demcheck::err_tdy_positive_integer_scalar(x = break_min,
                                                           name = "break_min",
                                                           null_ok = FALSE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = FALSE)
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    demcheck::err_gt_scalar(x1 = break_max,
                            x2 = break_min,
                            name1 = "break_max",
                            name2 = "break_min")
    if ((break_max - break_min) %% width != 0L)
        stop(gettextf("difference between '%s' [%d] and '%s' [%d] not divisible by '%s' [%d]",
                      "break_max", break_max, "break_min", break_min, "width", width))
    demcheck::err_is_logical_flag(x = recode_up,
                                  name = "recode_up")
    demcheck::err_is_logical_flag(x = recode_down,
                                  name = "recode_down")
    ## calculate age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## recode ages outside 'break_min', 'break_max', if requested
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(date)
    is_lt_min <- age_years < break_min
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        if (recode_up) {
            dob_ymd$y[is_lt_min] <- date_ymd$y[is_lt_min] - break_min
            dob_ymd$m[is_lt_min] <- date_ymd$m[is_lt_min]
            dob_ymd$d[is_lt_min] <- date_ymd$d[is_lt_min]
        }
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
        if (recode_down) {
            dob_ymd$y[is_ge_max] <- date_ymd$y[is_ge_max] - break_max + 1L
            dob_ymd$m[is_ge_max] <- date_ymd$m[is_ge_max]
            dob_ymd$d[is_ge_max] <- date_ymd$d[is_ge_max]
        }
        else {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'break_max' is %d and 'recode_down' is FALSE"),
                          date[[i_ge_max]],
                          dob[[i_ge_max]],
                          age_years[[i_ge_max]],
                          break_max))
        }
    }
    if ((i_lt_min > 0L) || (i_ge_max > 0L))
        dob <- as.Date(paste(dob_ymd$y,
                             dob_ymd$m,
                             dob_ymd$d,
                             sep = "-"))
    ## make time breaks
    dob_date <- sort(unique(c(dob, date)))
    breaks_time <- make_breaks_date_year(date = dob_date,
                                         month_start = month_start,
                                         width = width,
                                         origin = origin,
                                         break_min = NULL)
    ## make labels for time breaks
    labels_time <- make_labels_period(breaks = breaks_time,
                                      label_year_start = label_year_start,
                                      include_na = FALSE)
    ## make age breaks
    breaks_age <- make_breaks_integer_births(age = age_years,
                                             width = width,
                                             break_min = break_min,
                                             break_max = break_max)
    ## make labels for age breaks
    labels_age <- make_labels_age(breaks = breaks_age,
                                  open_last = FALSE)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "year",
                              breaks_time = breaks_time,
                              breaks_age = breaks_age,
                              open_last = FALSE,
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
#' \code{plot_date_to_triangle_quarter} is designed for
#' instruction or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_triangle_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 400.
#'
#' @examples
#' plot_date_to_triangle_quarter(date = c("2024-03-27",
#'                                        "2022-11-09"),
#'                               dob = "2020-01-01")
#'
#' ## open age group starts at 40 quarters
#' date_to_triangle_quarter(date = c("2017-03-27",
#'                                   "2024-03-27"),
#'                          dob = "2010-01-01",
#'                          break_max = 40)
#' @export
plot_date_to_triangle_quarter <- function(date,
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
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = show_months,
                                  name = "show_months")
    ## calculate age in months and quarters
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    ## if has upper limit, check that all
    ## ages less than limit
    if (!is.null(break_max) && !open_last) {
        demcheck::err_lt_break_max_age(age = age_quarters,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "month")
    }
    ## make time breaks
    dob_date <- sort(unique(c(dob, date)))
    breaks_time <- make_breaks_date_quarter(date = dob_date,
                                            break_min = NULL)
    break_min_time <- breaks_time[[1L]]
    break_max_time <- breaks_time[[length(breaks_time)]]
    ## make labels for time breaks
    labels_time <- make_labels_period_quarter(break_min = break_min_time,
                                              break_max = break_max_time,
                                              include_na = FALSE)
    ## make age breaks
    breaks_age <- make_breaks_integer_month_quarter(age = age_quarters,
                                                    break_max = break_max,
                                                    open_last = open_last)
    break_min_age <- breaks_age[[1L]]
    break_max_age <- breaks_age[[length(breaks_age)]]
    ## make labels for age breaks
    labels_age <- make_labels_age_quarter(break_min = break_min_age,
                                          break_max = break_max_age,
                                          open_last = open_last,
                                          include_na = FALSE)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "quarter",
                              breaks_time = breaks_time,
                              breaks_age = breaks_age,
                              open_last = open_last,
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
#' \code{plot_date_to_triangle_month} is designed for
#' instruction or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_triangle_year
#' @param break_max An integer or \code{NULL}.
#' Defaults to 1200.
#'
#' @examples
#' plot_date_to_triangle_month(date = c("2024-03-27",
#'                                      "2022-11-09"),
#'                             dob = "2020-01-01")
#'
#'
#' ## open age group starts at 120 months
#' plot_date_to_triangle_month(date = c("2017-03-27",
#'                                      "2024-03-27"),
#'                             dob = "2010-01-01",
#'                             break_max = 120)
#' @export
plot_date_to_triangle_month <- function(date,
                                        dob,
                                        break_max = 1200,
                                        open_last = TRUE) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## calculate age in months
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    ## if has upper limit, check that all
    ## ages less than limit
    if (!is.null(break_max) && !open_last) {
        demcheck::err_lt_break_max_age(age = age_months,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "month")
    }
    ## make time breaks
    dob_date <- sort(unique(c(dob, date)))
    breaks_time <- make_breaks_date_month(date = dob_date,
                                          break_min = NULL)
    break_min_time <- breaks_time[[1L]]
    break_max_time <- breaks_time[[length(breaks_time)]]
    ## make labels for time breaks
    labels_time <- make_labels_period_month(break_min = break_min_time,
                                            break_max = break_max_time,
                                            include_na = FALSE)
    ## make age breaks
    breaks_age <- make_breaks_integer_month_quarter(age = age_months,
                                                    break_max = break_max,
                                                    open_last = open_last)
    break_min_age <- breaks_age[[1L]]
    break_max_age <- breaks_age[[length(breaks_age)]]
    ## make labels for age breaks
    labels_age <- make_labels_age_month(break_min = break_min_age,
                                        break_max = break_max_age,
                                        open_last = open_last,
                                        include_na = FALSE)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "month",
                              breaks_time = breaks_time,
                              breaks_age = breaks_age,
                              open_last = open_last,
                              labels_time = labels_time,
                              labels_age = labels_age,
                              show_months = FALSE,
                              show_vert = TRUE,
                              show_diag = TRUE)
}

