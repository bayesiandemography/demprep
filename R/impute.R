
#' Impute a vector of dates
#'
#' Given years, and possibly months and days, 
#' randomly generate a dates. 
#' The dates are generated
#' from a uniform distribution:
#' all dates that fall within the range implied by 
#' the \code{year}, \code{month}, and \code{day} arguments
#' have the same chance of being selected. 
#' For instance, if \code{year}
#' is \code{2000}, \code{month} is \code{"March"}, and
#' no value is supplied for \code{date}, then every date from
#' \code{"2000-03-01"} to \code{"2000-03-31"} has the same
#' 1/31 chance of being selected.
#'
#' Months can be specified in one of four ways:
#' \enumerate{
#'   \item Numbers \code{1}, \code{2}, \dots, \code{12}
#'   \item Codes \code{"01"}, \code{"02"}, \dots, \code{"12"}
#'   \item English full names: \code{"January"}, \code{"February"}, \dots,
#'     \code{"December"}
#'   \item English abbreviations: \code{"Jan"}, \code{"Feb"}, \dots, 
#'     \code{"Dec"}
#' }
#'
#' \code{month} and \code{day}, if supplied, must have the
#' same length as \code{year}.
#' 
#' @param year A numeric vector specifying years. Required.
#' @param month A numeric or character vector
#' specifying months. Optional.
#' @param day A numeric vector specifying days. Optional
#' 
#' @return A vector of class \code{\link[base]{Date}}.
#'
#' @seealso \code{\link{impute_dob}}
#'
#' @examples
#' impute_date(year = 2000:2004)
#' impute_date(year = 2000:2004,
#'             month = c("Feb", "Jun", "May", "Apr", "Feb"))
#' impute_date(year = 2000:2004,
#'             day = c(3, 31, 11, 2, NA))
#' @export
impute_date <- function(year, month = NULL, day = NULL) {
    has_month <- !is.null(month)
    has_day <- !is.null(day)
    ## check and tidy 'year'
    year <- demcheck::err_tdy_integer_vector(x = year,
                                             name = "year")
    n <- length(year)
    ## check and tidy 'month'
    if (has_month) {
        demcheck::err_length_same(x1 = year,
                                  x2 = month,
                                  name1 = year,
                                  name2 = month)
        month <- demcheck::err_tdy_month(x = month,
                                         name = "month")
    }
    ## check and tidy 'day'
    if (has_day) {
        demcheck::err_length_same(x1 = year,
                                  x2 = day,
                                  name1 = year,
                                  name2 = day)
        day <- demcheck::err_tdy_integer_vector(x = day,
                                                name = "day")
        demcheck::err_integer_in_range(x = month,
                                       min = 1L,
                                       max = 31L,
                                       name = "month")
    }
    ## handle case where 'month' and 'day' both supplied
    if (has_month && has_day) {
        ans <- sprintf("%s-%s-%s", year, month, day)
        as.Date(ans)
    }
    ## handle remaining cases where 'month' and/or 'day' imputed
    else if (has_month && !has_day) {
        impute_date_day(year = year,
                        month = month)
    }
    else if (!has_month && has_day) {
        impute_date_month(year = year,
                          day = day)
    }
    else {
        impute_date_month_day(year = year)
    }
}

#' Impute a vector of dates of birth
#'
#' Given dates and ages, randomly generate 
#' dates of birth. Ages
#' are measured in complete years and/or completed
#' months. The dates of birth are generated from a 
#' uniform distribution: all dates that fall within 
#' the range implied by the \code{date},
#' \code{age_years}, and \code{age_months} 
#' arguments have the same chance of being selected.
#' For instance, if \code{date} is \code{"2000-12-31"}
#' and \code{age} is \code{0}, then every date from
#' \code{"2000-01-01"} to \code{"2000-12-31"}
#' has the same 1/366 chance of being selected.
#'
#' The range of possible dates depends on whether
#' age is specified in years or months.
#' If age is specified in years only, then
#' the range is one year. If age is specified in months,
#' or in a combination of years and months,
#' then the range is one month. See below for an example.
#' 
#' ADD DISCUSSION OF SUBTLETIES OF MEASURING AGE
#'
#' \code{date} must have class \code{\link[base]{Date}},
#' or can be coerced to class \code{Date}
#' via function \code{\link[base]{as.Date}}.
#'
#' \code{date} and \code{age} must have the same length.
#'
#' @param date A vector of dates on which age is measured.
#' @param age_years Age measured in years.
#' @param age_months Age measured in months.
#' 
#' @return A vector of class \code{\link[base]{Date}}
#'
#' @seealso \code{\link{impute_date}}
#'
#' @examples
#' impute_dob(date = c("2000-06-30", "2001-01-01"),
#'            age_years = c(4, 10))
#' impute_dob(date = c("2000-06-30", "2001-01-01"),
#'            age_months = c(3, 11))
#' impute_dob(date = c("2000-06-30", "2001-01-01"),
#'            age_years = c(0, 4),
#'            age_months = c(3, 11))
#' ## if age is specified in months, then the range of
#' ## possible dates of birth is narrower than if it
#' ## is specified in years
#' ans_years <- impute_dob(date = rep("2020-01-01", 100),
#'                         age_years = 1)
#' ans_months <- impute_dob(date = rep("2020-01-01", 100),
#'                          age_months = 12)
#' range(ans_years) # up to one year
#' range(ans_months) # up to one month
#' @export
impute_dob <- function(date, age_years = NULL, age_months = NULL) {
    has_years <- !is.null(age_years)
    has_months <- !is.null(age_months)
    ## check and tidy 'date'
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    ## check that at least one of 'age_years' 
    ## and 'age_months' is supplied
    if (is.null(age_years) && is.null(age_months))
        stop(gettextf("one or both of '%s' and '%s' must be supplied",
                      "age_years", "age_months"))
    ## check and tidy 'age_years'
    if (has_years) {
        age_years <- demcheck::err_tdy_integer_vector(x = age_years,
                                                      name = "age_years")
        demcheck::err_non_negative_vector(x = age_years,
                                          name = "age_years")
    }
    ## check and tidy 'month'
    if (has_months) {
        age_months <- demcheck::err_tdy_integer_vector(x = age_months,
                                                       name = "age_months")
        demcheck::err_non_negative_vector(x = age_months,
                                          name = "age_months")
    }
    ## check that 'age_years' and 'age_months' have same length
    if (has_years && has_months)
        demcheck::err_length_same(x1 = age_years,
                                  x2 = age_months,
                                  name1 = "age_years",
                                  name2 = "age_months")
    ## do imputation
    if (has_months) {
        if (has_years)
            age_months <- age_months + 12L * age_years
        date_max <- add_months(date = date,
                               n = -age_months)
        date_min_minus_one  <- add_months(date = date,
                                          n = -(age_months + 1L))
    }
    else {
        date_max <- add_years(date = date,
                              n = -age_years)
        date_min_minus_one <- add_years(date = date_max,
                                        n = -1L)
    }
    diff <- as.integer(date_max - date_min_minus_one)
    n <- length(diff)
    offset <- as.integer(stats::runif(n) * diff)
    at_upper_limit <- !is.na(offset) & (offset == diff) ## unlikely, but check anyway
    offset[at_upper_limit] <- offset[at_upper_limit] - 1L
    ans <- date_min_minus_one + (offset + 1L) 
    ans
}
