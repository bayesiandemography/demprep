
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
#' @export
plot_date_to_age_year <- function(date,
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
#' function 'date_to_age_multi'
#'
#' Create a plot illustrating how function
#' \code{\link{date_to_age_multi}} works.
#' 
#' \code{plot_date_to_age_multi} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_age_year
#' @param width The width in years of the age intervals.
#' A positive integer. Defaults to 5.
#'
#' @examples
#' plot_date_to_age_multi(date = c("2024-03-27",
#'                                 "2022-11-09"),
#'                        dob = c("2001-03-21",
#'                                "2013-07-13"),
#'                        break_max = 35)
#'
#' ## alternative values for 'width' and 'break_max'
#' plot_date_to_age_multi(date = c("2024-03-27",
#'                                 "2022-11-09"),
#'                        dob = c("2001-03-21",
#'                                "2013-07-13"),
#'                        width = 10,
#'                        break_max = 40)
#'
#' ## oldest age group closed
#' plot_date_to_age_multi(date = c("2024-03-27",
#'                                 "2022-11-09"),
#'                        dob = c("2001-03-21",
#'                                "2013-07-13"),
#'                        width = 10,
#'                        break_max = 40,
#'                        open_last = FALSE)
#' @export
plot_date_to_age_multi <- function(date,
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
    if (!is.null(break_max) && !open_last)
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
    labels <- make_labels_age(breaks = breaks,
                              open_last = open_last)

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
#' function 'date_to_age_lifetab'
#'
#' Create a plot illustrating how function
#' \code{\link{date_to_age_lifetab}} works.
#' 
#' \code{plot_date_to_age_lifetab} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_age_year
#' @param date Date of death.
#'
#' @examples
#' plot_date_to_age_lifetab(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          dob = c("2001-03-21",
#'                                  "2004-07-13"),
#'                          break_max = 30)
#' @export
plot_date_to_age_lifetab <- function(date,
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
    labels <- make_labels_age(breaks = breaks,
                              open_last = TRUE,
                              include_na = FALSE)
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "year",
                              breaks_time = NULL,
                              breaks_age = breaks,
                              open_last = TRUE,
                              labels_time = NULL,
                              labels_age = labels,
                              show_months = show_months,
                              show_vert = FALSE,
                              show_diag = FALSE)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_births'
#'
#' Create a plot illustrating how function
#' \code{\link{date_to_age_births}} works.
#' 
#' \code{plot_date_to_age_births} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_age_year
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
#' plot_date_to_age_births(date = c("2024-03-27",
#'                                  "2022-11-09"),
#'                         dob = c("2001-03-21",
#'                                 "2000-07-13"))
#' plot_date_to_age_births(date = c("2024-03-27",
#'                                  "2022-11-09"),
#'                         dob = c("2001-03-21",
#'                                 "2000-07-13"),
#'                         width = 10,
#'                         break_min = 20)
#'
#' ## allow youngest and oldest age groups to be
#' ## set by the data
#' plot_date_to_age_births(date = c("2052-01-02",
#'                                  "2019-09-22",
#'                                  "2022-10-08"),
#'                         dob = c("2000-01-01",
#'                                 "2001-03-17",
#'                                 "2010-07-05"),
#'                         break_min = NULL,
#'                         break_max = NULL)
#'
#' ## recode ages outside the expected range
#' plot_date_to_age_births(date = c("2052-01-02",
#'                                  "2019-09-22",
#'                                  "2022-10-08"),
#'                         dob = c("2000-01-01",
#'                                 "2001-03-17",
#'                                 "2010-07-05"),
#'                         recode_up = TRUE,
#'                         recode_down = TRUE)
#' @export
plot_date_to_age_births <- function(date, dob,
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
            if (recode_up) {
                age_years[is_lt_min] <- break_min
                dob[is_lt_min] <- add_years(date[is_lt_min], n = -break_min)
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
    }
    if (!is.null(break_max)) {
        is_ge_max <- age_years >= break_max
        i_ge_max <- match(TRUE, is_ge_max, nomatch = 0L)
        if (i_ge_max > 0L) {
            if (recode_down) {
                age_years[is_ge_max] <- break_max - 1L
                dob[is_ge_max] <- add_years(date[is_ge_max], n = -break_max) + 1L
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
    }
    ## make breaks
    breaks <- make_breaks_integer_births(age = age_years,
                                         width = width,
                                         break_min = break_min,
                                         break_max = break_max)
    ## make labels for breaks
    labels <- make_labels_age(breaks = breaks,
                              open_last = FALSE)
    ## make plot
    plot_date_to_age_triangle(date = date,
                              dob = dob,
                              unit = "year",
                              breaks_time = NULL,
                              breaks_age = breaks,
                              open_last = FALSE,
                              labels_time = NULL,
                              labels_age = labels,
                              show_months = show_months,
                              show_vert = FALSE,
                              show_diag = FALSE)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_age_custom'
#'
#' Create a plot illustrating how function
#' \code{\link{date_to_age_custom}} works.
#' 
#' \code{plot_date_to_age_custom} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_age_year
#' @param breaks A vector of strictly increasing integer values.
#'
#' @examples
#' plot_date_to_age_custom(date = c("2024-03-27",
#'                                  "2022-11-09"),
#'                         dob = c("2001-03-21",
#'                                 "2010-07-13"),
#'                         breaks = c(0, 15, 60))
#' plot_date_to_age_custom(date = c("2024-03-27",
#'                                  "2022-11-09"),
#'                         dob = c("2001-03-21",
#'                                 "2010-07-13"),
#'                         breaks = c(10, 40, 65))
#'
#' ## alternative specifications for oldest age group
#' plot_date_to_age_custom(date = c("2024-03-27",
#'                                  "2022-11-09"),
#'                         dob = c("2001-03-21",
#'                                 "2010-07-13"),
#'                         breaks = c(0, 15, 50))
#' plot_date_to_age_custom(date = c("2024-03-27",
#'                                  "2022-11-09"),
#'                         dob = c("2001-03-21",
#'                                 "2010-07-13"),
#'                         breaks = c(0, 15, 50),
#'                         open_last = FALSE)
#' @export
plot_date_to_age_custom <- function(date, dob,
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
#' @export
plot_date_to_age_quarter <- function(date,
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
    labels <- make_labels_age_quarter(break_min = 0L,
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
#' @export
plot_date_to_age_month <- function(date,
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
    labels <- make_labels_age_month(break_min = 0L,
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
