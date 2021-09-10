
## HAS_TESTS
#' Convert dates to one-year Lexis triangles
#'
#' Use dates of events and dates of birth
#' to create one-year Lexis triangles.
#'
#' See \code{vignette("demprep")} for the definition
#' of Lexis triangles.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' Periods start on the first day of \code{month_start},
#' and end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December.
#'
#' @param date Dates of events.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class
#' \code{Date} using function \code{\link[base]{as.Date}}.
#' @param dob Dates of birth.
#' A vector of class \code{\link[base]{Date}},
#' or a vector that can be coerced to class
#' \code{Date} using function \code{\link[base]{as.Date}}.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. The period starts on
#' the first day of this month.
#'
#' @return A character vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_triangle_year}
#' is often processed further using function
#' \code{\link{format_triangle_year}}.
#'
#' Other functions for creating
#' triangles from dates are
#' \code{\link{date_to_triangle_quarter}}
#' and \code{\link{date_to_triangle_month}}.
#'
#' Other functions for creating one-year
#' units from dates are
#' \code{\link{date_to_age_year}}.
#' \code{\link{date_to_period_year}}, and
#' \code{\link{date_to_cohort_year}}.
#'
#' @examples
#' date_to_triangle_year(date = c("2024-03-27",
#'                                "2022-11-09"),
#'                       dob = "2020-01-01")
#'
#' ## starts on 1 July rather than 1 January
#' date_to_triangle_year(date = c("2024-03-27",
#'                                "2022-11-09"),
#'                       dob = "2020-01-01",
#'                       month_start = "Jul")
#' 
#' ## events occurring to people born during the
#' ## period in question are always allocated to
#' ## lower Lexis triangles
#' date_to_triangle_year(date = c("2020-03-19",
#'                                "2020-06-18"),
#'                       dob = c("2020-03-01",
#'                               "2020-06-18"))
#' @export
date_to_triangle_year <- function(date, dob, month_start = "Jan") {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    ## calculate age in months and years
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    ## prepare to assign triangles
    ans <- rep(NA_character_, times = length(date))
    date_start_year <- rollback_year(date = date,
                                     month_start = month_start)
    ## (subtract one day so that events occurring to people who
    ## attain age on first day of year are coded as "Lower")
    date_before_year <- date_start_year - 1L
    age_in_months_before_year <- age_completed_months(date = date_before_year,
                                                      dob = dob)
    age_in_years_before_year <- age_in_months_before_year %/% 12L
    ## assign triangles - born during period
    born_during_year <- (!is.na(dob)
        & !is.na(date_before_year)
        & (dob > date_before_year)) ## includes first day of period
    ans[born_during_year] <- "Lower"
    ## assign triangles - remaining age groups
    is_unassigned <- (!born_during_year
        & !is.na(age_in_years_before_year)
        & !is.na(age_years))
    diff_age <- age_years - age_in_years_before_year
    if (any(is_unassigned & !(diff_age %in% c(0L, 1L))))
        stop("invalid value for 'diff_age'")
    is_lower <- is_unassigned & (age_years == age_in_years_before_year + 1L)
    is_upper <- is_unassigned & !is_lower
    ans[is_lower] <- "Lower"
    ans[is_upper] <- "Upper"
    ans
}



## HAS_TESTS
#' Convert dates to one-quarter (three-month) Lexis triangles
#'
#' Use dates of events and dates of birth to create
#' one-quarter (three-month) Lexis triangles.
#'
#' See \code{vignette("demprep")} for the definition
#' of Lexis triangles.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' @inheritParams date_to_triangle_year
#'
#' @return A character vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_triangle_quarter}
#' is often processed further using function
#' \code{\link{format_triangle_quarter}}.
#'
#' Other functions for creating
#' triangles from dates are
#' \code{\link{date_to_triangle_year}}
#' and \code{\link{date_to_triangle_month}}.
#'
#' Other functions for creating one-quarter
#' units from dates are
#' \code{\link{date_to_age_quarter}}.
#' \code{\link{date_to_period_quarter}}, and
#' \code{\link{date_to_cohort_quarter}}.
#'
#' @examples
#' date_to_triangle_quarter(date = c("2024-03-27",
#'                                   "2022-11-09"),
#'                          dob = "2020-01-01")
#' @export
date_to_triangle_quarter <- function(date, dob) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    ## calculate age in months and quarters
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_quarters <- age_months %/% 3L
    ## prepare to assign triangles
    ans <- rep(NA_character_, times = length(date))
    date_start_quarter <- rollback_quarter(date)
    ## (subtract one day so that events occurring to people who
    ## attain age on first day of quarter are coded as "Lower")
    date_before_quarter <- date_start_quarter - 1L
    age_in_months_before_quarter <- age_completed_months(date = date_before_quarter,
                                                         dob = dob)
    age_in_quarters_before_quarter <- age_in_months_before_quarter %/% 3L
    ## assign triangles - born during quarter
    born_during_quarter <- (!is.na(dob)
        & !is.na(date_before_quarter)
        & (dob > date_before_quarter)) ## includes first day of quarter
    ans[born_during_quarter] <- "Lower"
    ## assign triangles - remaining age groups
    is_unassigned <- (!born_during_quarter
        & !is.na(age_in_quarters_before_quarter)
        & !is.na(age_quarters))
    diff_age <- age_quarters - age_in_quarters_before_quarter
    if (any(is_unassigned & !(diff_age %in% c(0L, 1L))))
        stop("invalid value for 'diff_age'")
    is_lower <- is_unassigned & (age_quarters == age_in_quarters_before_quarter + 1L)
    is_upper <- is_unassigned & !is_lower
    ans[is_lower] <- "Lower"
    ans[is_upper] <- "Upper"
    ans
}

## HAS_TESTS
#' Convert dates to one-month Lexis triangles
#'
#' Use dates of events and dates of birth to create
#' one-month Lexis triangles.
#'
#' See \code{vignette("demprep")} for the definition
#' of Lexis triangles.
#'
#' \code{date} and \code{dob} must have the same length,
#' unless one of them has length 1, in which case the
#' length-1 argument is recycled.
#'
#' @inheritParams date_to_triangle_year
#'
#' @return A character vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_triangle_month}
#' is often processed further using function
#' \code{\link{format_triangle_month}}.
#'
#' Other functions for creating
#' triangles from dates are
#' \code{\link{date_to_triangle_year}}
#' and \code{\link{date_to_triangle_quarter}}.
#'
#' Other functions for creating one-month
#' units from dates are
#' \code{\link{date_to_age_month}}.
#' \code{\link{date_to_period_month}}, and
#' \code{\link{date_to_cohort_month}}.
#' 
#' @examples
#' date_to_triangle_month(date = c("2024-03-27",
#'                                 "2022-11-09"),
#'                        dob = "2020-01-01")
#' @export
date_to_triangle_month <- function(date, dob) {
    ## Check arguments and/or apply defaults.
    ## Note that 'err_tdy_date_dob' enforces length >= 1
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    ## calculate age in months
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    ## prepare to assign triangles
    ans <- rep(NA_character_, times = length(date))
    date_start_month <- rollback_month(date)
    ## (subtract one day so that events occurring to people who
    ## attain age on first day of month are coded as "Lower")
    date_before_month <- date_start_month - 1L
    age_in_months_before_month <- age_completed_months(date = date_before_month,
                                                    dob = dob)
    ## assign triangles - born during month
    born_during_month <- (!is.na(dob)
        & !is.na(date_before_month)
        & (dob > date_before_month)) ## includes first day of month
    ans[born_during_month] <- "Lower"
    ## assign triangles - remaining age groups
    is_unassigned <- (!born_during_month
        & !is.na(age_in_months_before_month)
        & !is.na(age_months))
    diff_age <- age_months - age_in_months_before_month
    if (any(is_unassigned & !(diff_age %in% c(0L, 1L))))
        stop("invalid value for 'diff_age'")
    is_lower <- is_unassigned & (age_months == age_in_months_before_month + 1L)
    is_upper <- is_unassigned & !is_lower
    ans[is_lower] <- "Lower"
    ans[is_upper] <- "Upper"
    ans
}
