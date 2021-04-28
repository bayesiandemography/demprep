
## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_cohort_year'
#' 
#' Create a plot illustrating how function
#' \code{\link{date_to_cohort_year}} works.
#' 
#' \code{plot_date_to_cohort_year} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @param date Dates of events or measurements.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The cohort starts on
#' the first day of this month.
#' @param label_year_start Whether to label a cohort
#' by the calendar year at the beginning of the cohort
#' or the calendar year at the end. Not needed for cohorts
#' that start on 1 January. Defaults to \code{TRUE}.
#'
#' @examples
#' plot_date_to_cohort_year(date = c("2024-03-27",
#'                                   "2022-11-09"))
#'
#' ## starts on 1 July rather than 1 January
#' plot_date_to_cohort_year(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          month_start = "Jul")
#'
#' ## starts on 1 July rather than 1 January,
#' ## and uses calendar year at end rather than
#' ## calendar year at the beginning
#' plot_date_to_cohort_year(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          month_start = "Jul",
#'                          label_year_start = FALSE)
#'
#' ## specify oldest cohort, with 'open_first'
#' ## at default value of 'TRUE', so first cohort
#' ## has no lower limit
#' @keywords internal
#' @export
plot_date_to_cohort_year <- function(date,
                                     month_start = "Jan",
                                     label_year_start = TRUE) {
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    if (has_break_min) {
        demcheck::err_length_1(x = break_min,
                               name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
        month_start <- format(break_min, format = "%b")
    }
    else {
        month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                     name = "month_start")
    }
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
        if (open_first && !has_break_min)
            stop(gettextf("'%s' is %s but '%s' is %s",
                          "open_first", "TRUE", "break_min", "NULL"))
        if (open_first && !isTRUE(label_year_start))
            stop(gettextf("'%s' is %s but '%s' is %s",
                          "open_first", "TRUE", "label_year_start", "FALSE"))
    }
    else        
        open_first <- isTRUE(label_year_start) && has_break_min
    if (!open_first && has_break_min)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_year(date = date,
                                    month_start = month_start)
    ## create labels
    labels <- make_labels_cohort(breaks = breaks,
                                 open_first = open_first,
                                 label_year_start = label_year_start,
                                 include_na = FALSE)
    ## make plot
    plot_date_to_cohort(date = date,
                        breaks = breaks,
                        open_first = open_first,
                        labels = labels)
}


## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_cohort_quarter'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_cohort_quarter}} works.
#' \code{plot_date_to_cohort_quarter} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_cohort_year
#'
#' @examples
#' plot_date_to_cohort_quarter(date = c("2024-03-27",
#'                                      "2022-05-13",
#'                                      "2022-11-09"))
#' plot_date_to_cohort_quarter(date = c("2024-03-27",
#'                                      "2022-05-13",
#'                                      "2022-11-09"),
#'                             break_min = "2022-01-01")
#' @keywords internal
#' @export
plot_date_to_cohort_quarter <- function(date,
                                        break_min = NULL,
                                        open_first = NULL) {
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    if (has_break_min) {
        demcheck::err_length_1(x = break_min,
                               name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
    }
    if (has_open_first)
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    else
        open_first <- has_break_min
    if (!open_first && has_break_min)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_quarter(date = date,
                                               break_min = break_min,
                                               has_break_min_arg = TRUE)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_cohort_quarter(break_min = break_min,
                                         break_max = break_max,
                                         open_first = open_first,
                                         include_na = FALSE)
    ## make plot
    plot_date_to_cohort(date = date,
                        breaks = breaks,
                        open_first = open_first,
                        labels = labels)
}

## NO_TESTS
#' Depict the intervals created by
#' function 'date_to_cohort_month'
#'
#' Create plot illustrating how function
#' \code{\link{date_to_cohort_month}} works.
#' 
#' \code{plot_date_to_cohort_month} is typically used for
#' learning or documentation, rather than for
#' actual data analysis.
#'
#' @inheritParams plot_date_to_cohort_year
#'
#' @examples
#' plot_date_to_cohort_month(date = c("2024-03-27",
#'                                    "2023-08-13",
#'                                    "2023-11-09"))
#' plot_date_to_cohort_month(date = c("2024-03-27",
#'                                    "2023-08-13",
#'                                    "2023-11-09"),
#'                           break_min = "2023-10-01")
#' @keywords internal
#' @export
plot_date_to_cohort_month <- function(date,
                                      break_min = NULL,
                                      open_first = NULL) {
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments and/or apply defaults
    demcheck::err_positive_length(x = date,
                                  name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    if (has_break_min) {
        demcheck::err_length_1(x = break_min,
                               name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
    }
    if (has_open_first)
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    else
        open_first <- has_break_min
    if (!open_first && has_break_min)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_month(date = date,
                                             break_min = break_min,
                                             has_break_min_arg = TRUE)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_cohort_month(break_min = break_min,
                                       break_max = break_max,
                                       open_first = open_first,
                                       include_na = FALSE)
    ## make plot
    plot_date_to_cohort(date = date,
                        breaks = breaks,
                        open_first = open_first,
                        labels = labels)
}
