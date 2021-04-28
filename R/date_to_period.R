
## HAS_TESTS
#' Convert dates to one-year periods
#'
#' Allocate dates to one-year periods. The one-year periods are,
#' by default, calendar years.
#' 
#' Periods start on the first day of \code{month_start},
#' and end one-year-minus-one-day later.
#' The default value for \code{month_start} is \code{"Jan"},
#' so periods by default start on 1 January and
#' end on 31 December. \code{month_start} can be a
#' full month name or an abbreviation.
#'
#' If a period starts on 1 January, then the first day and last day
#' of the period belong to the same calendar year.
#' But if a period starts on any other day, then
#' the first day belongs to one calendar
#' year and the last day belongs to the next calendar year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, then the first
#' day belongs to calendar year 2000, and the last
#' day belongs to calendar year 2001. Some people
#' use the first year to label such periods,
#' and others use the last year.
#' For instance, if a period extends from
#' 1 July 2000 to 30 June 2001, some people
#' label the period \code{"2000"}, and
#' others label it \code{"2001"}. Function
#' \code{date_to_period_year} by default uses
#' the start year. To use the end year, set
#' \code{label_year_start} to \code{FALSE}.
#'
#' Here, for different settings of
#' \code{month_start} and \code{label_year_start},
#' is how \code{date_to_period_year}
#' constructs a label for an event occurring
#' on 15 June 2020:
#'
#' \tabular{cccc}{
#'   \code{month_start} \tab \code{label_year_start} \tab Period \tab Label \cr
#'   \code{"Jan"} \tab \code{TRUE}  \tab 1 Jan 2020 - 31 Dec 2020 \tab \code{"2020"} \cr
#'   \code{"Jan"} \tab \code{FALSE} \tab 1 Jan 2020 - 31 Dec 2020 \tab \code{"2020"} \cr
#'   \code{"Feb"} \tab \code{TRUE}  \tab 1 Feb 2020 - 31 Jan 2021 \tab \code{"2020"} \cr
#'   \code{"Feb"} \tab \code{FALSE} \tab 1 Feb 2020 - 31 Jan 2021 \tab \code{"2021"} \cr
#'   \code{"Mar"} \tab \code{TRUE}  \tab 1 Mar 2020 - 28 Feb 2021 \tab \code{"2020"} \cr
#'   \code{"Mar"} \tab \code{FALSE} \tab 1 Mar 2020 - 28 Feb 2021 \tab \code{"2021"} \cr
#'   \code{"Apr"} \tab \code{TRUE}  \tab 1 Apr 2020 - 31 Mar 2021 \tab \code{"2020"} \cr
#'   \code{"Apr"} \tab \code{FALSE} \tab 1 Apr 2020 - 31 Mar 2021 \tab \code{"2021"} \cr
#'   \code{"May"} \tab \code{TRUE}  \tab 1 May 2020 - 30 Apr 2021 \tab \code{"2020"} \cr
#'   \code{"May"} \tab \code{FALSE} \tab 1 May 2020 - 30 Apr 2021 \tab \code{"2021"} \cr
#'   \code{"Jun"} \tab \code{TRUE}  \tab 1 Jun 2020 - 31 May 2021 \tab \code{"2020"} \cr
#'   \code{"Jun"} \tab \code{FALSE} \tab 1 Jun 2020 - 31 May 2021 \tab \code{"2021"} \cr
#'   \code{"Jul"} \tab \code{TRUE}  \tab 1 Jul 2019 - 30 Jun 2020 \tab \code{"2019"} \cr
#'   \code{"Jul"} \tab \code{FALSE} \tab 1 Jul 2019 - 30 Jun 2020 \tab \code{"2020"} \cr
#'   \code{"Aug"} \tab \code{TRUE}  \tab 1 Aug 2019 - 31 Jul 2020 \tab \code{"2019"} \cr
#'   \code{"Aug"} \tab \code{FALSE} \tab 1 Aug 2019 - 31 Jul 2020 \tab \code{"2020"} \cr
#'   \code{"Sep"} \tab \code{TRUE}  \tab 1 Sep 2019 - 31 Aug 2020 \tab \code{"2019"} \cr
#'   \code{"Sep"} \tab \code{FALSE} \tab 1 Sep 2019 - 31 Aug 2020 \tab \code{"2020"} \cr
#'   \code{"Oct"} \tab \code{TRUE}  \tab 1 Oct 2019 - 30 Sep 2020 \tab \code{"2019"} \cr
#'   \code{"Oct"} \tab \code{FALSE} \tab 1 Oct 2019 - 30 Sep 2020 \tab \code{"2020"} \cr
#'   \code{"Nov"} \tab \code{TRUE}  \tab 1 Nov 2019 - 31 Oct 2020 \tab \code{"2019"} \cr
#'   \code{"Nov"} \tab \code{FALSE} \tab 1 Nov 2019 - 31 Oct 2020 \tab \code{"2020"} \cr
#'   \code{"Dec"} \tab \code{TRUE}  \tab 1 Dec 2019 - 30 Nov 2020 \tab \code{"2019"} \cr
#'   \code{"Dec"} \tab \code{FALSE} \tab 1 Dec 2019 - 30 Nov 2020 \tab \code{"2020"}
#' }
#' 
#' @param date Dates of events or measurements.
#' A vector of class \code{\link[base]{Date}},
#' or a vector than can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#' @param month_start An element of \code{\link[base]{month.name}},
#' or \code{\link[base]{month.abb}}. Each period starts on
#' the first day of this month.
#' @param label_year_start Whether to label a period
#' by the calendar year at the beginning of the period
#' or the calendar year at the end. Defaults to \code{TRUE}.
#'
#' @return An integer vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_period_year}
#' is often processed further using \code{\link{format_period_year}}.
#'
#' Other functions for creating periods from dates are
#' \code{\link{date_to_period_quarter}} and
#' and \code{\link{date_to_period_month}}.
#'
#' Other functions for creating one-year units
#' from dates are
#' \code{\link{date_to_age_year}},
#' \code{\link{date_to_cohort_year}},
#' and \code{\link{date_to_triangle_year}}.
#' The interface for \code{date_to_period_year} is identical
#' to that of \code{\link{date_to_cohort_year}}.
#'
#' @examples
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"))
#'
#' ## period starts on 1 July, not 1 January
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul")
#'
#' ## period starts on 1 July, using the calendar year at
#' ## the end, rather than beginning, for the label
#' date_to_period_year(date = c("2024-03-27", "2022-11-09"),
#'                     month_start = "Jul",
#'                     label_year_start = FALSE)
#' @export
date_to_period_year <- function(date,
                                month_start = "Jan",
                                label_year_start = TRUE) {
    date_to_cohort_period_year(date = date,
                               month_start = month_start,
                               label_year_start = label_year_start)
}


## HAS_TESTS
#' Convert dates to quarter (three-month) periods
#'
#' Allocate dates to periods. The
#' periods have widths of one quarter.
#' Quarters are defined as follows:
#' \tabular{lll}{
#'   \strong{Quarter} \tab \strong{Start} \tab \strong{End} \cr
#'   Q1 \tab 1 January \tab 31 March \cr
#'   Q2 \tab 1 April \tab 30 June \cr
#'   Q3 \tab 1 July \tab 30 September \cr
#'   Q4 \tab 1 October \tab 31 December
#' }
#'
#' @inheritParams date_to_period_year
#'
#' @return A character vector with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_period_quarter}
#' is often processed further using \code{\link{format_period_quarter}}.
#'
#' Other functions for creating periods from dates are
#' \code{\link{date_to_period_year}} and
#' and \code{\link{date_to_period_month}}.
#'
#' Other functions for creating one-quarter units
#' from dates are
#' \code{\link{date_to_age_quarter}},
#' \code{\link{date_to_cohort_quarter}},
#' and \code{\link{date_to_triangle_quarter}}.
#' The interface for \code{date_to_period_quarter} is identical
#' to that of \code{\link{date_to_cohort_quarter}}.
#' 
#' @examples
#' date_to_period_quarter(date = c("2024-03-27",
#'                                 "2020-01-03",
#'                                 "2022-11-09"))
#' @export
date_to_period_quarter <- function(date) {
    ## see if arguments supplied
    has_date <- sum(!is.na(date)) > 0L
    ## check arguments and/or apply defaults
    if (has_date)
        date <- demcheck::err_tdy_date_vector(x = date,
                                              name = "date")
    ## deal with "empty" case where 'date' has length 0
    ## or is all NA
    if (!has_date) {
            if (length(date) > 0L)
                ans <- factor(date,
                              levels = NA_character_,
                              exclude = NULL)
            else
                ans <- factor()
        return(ans)
    }
    ## create sequence of breaks
    breaks <- make_breaks_date_to_date_quarter(date = date,
                                               break_min = NULL,
                                               has_break_min_arg = FALSE)
    ## make labels for these breaks
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    include_na <- anyNA(date)
    labels <- make_labels_period_quarter(break_min = break_min,
                                         break_max = break_max,
                                         include_na = include_na)
    ## assign labels to dates
    i <- findInterval(x = date,
                      vec = breaks)
    ans <- labels[i]
    ## return result
    ans <- factor(x = ans,
                  levels = labels,
                  exclude = NULL)
    ans   
}

## HAS_TESTS (via date_to_cohort_period_month)
#' Convert dates to one-month periods
#'
#' Allocate dates to periods with month-long
#' periods.
#'
#' @inheritParams date_to_period_year
#'
#' @return A factor with the same length as \code{date}.
#'
#' @seealso The output from \code{date_to_period_month}
#' is often processed further using \code{\link{format_period_month}}.
#'
#' Other functions for creating periods from dates are
#' \code{\link{date_to_period_year}} and
#' and \code{\link{date_to_period_quarter}}.
#'
#' Other functions for creating one-month units
#' from dates are
#' \code{\link{date_to_age_month}},
#' \code{\link{date_to_cohort_month}},
#' and \code{\link{date_to_triangle_month}}.
#' The interface for \code{date_to_period_month} is identical
#' to that of \code{\link{date_to_cohort_month}}.
#'
#' @examples
#' date_to_period_month(date = c("2024-03-27",
#'                               "2020-01-03",
#'                               "2022-11-09"))
#' @export
date_to_period_month <- function(date) {
    date_to_cohort_period_month(date = date)
}

