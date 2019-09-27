#' Use dates and dates of birth to calculate Lexis triangles
#' 
#' @name date_to_triangle
NULL

#' @rdname date_to_triangle
#' @export
date_to_triangle_year <- function(date,
                                  dob,
                                  break_max = 100,
                                  open_right = TRUE,
                                  first_month = "Jan",
                                  year_to = TRUE,
                                  as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    first_month <- demcheck::err_tdy_first_month(x = first_month,
                                                 name = "first_month")
    demcheck::err_is_logical_flag(x = year_to,
                                  name = "year_to")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- date_to_age_completed_months(date = date,
                                               dob = dob)
    age_years <- age_months %/% 12L
    if (!open_right)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    n <- length(date)
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    is_lower_within_month <- is_lower_within_month(date_ymd = date_ymd,
                                                   dob_ymd = dob_ymd)
    month_date <- date_ymd$m
    month_dob <- dob_ymd$d
    is_lower <- ((month_date > month_dob)
        | ((month_date == month_dob) & is_lower_within_month))
    ans[is_lower] <- "Lower"
    may_have_ages_above_break_max <- !is.null(break_max) && open_right
    if (may_have_ages_above_break_max) {
        age_start <- age_completed_months_start_month(date_ymd = date_ymd,
                                                dob_ymd = dob_ymd)
        is_open_upper <- !is.na(age_start) & (age_start >= break_max)
        ans[is_open_upper] <- "Upper"
    }
    if (as_factor) {
        levels <- c("Lower", "Upper")
        has_na <- any(is.na(date)) || any(is.na(dob))
        if (has_na)
            levels <- c(levels, NA)
        ans <- factor(ans,
                      levels = levels,
                      exclude = NULL)
    }
    ans
}

#' @rdname date_to_triangle
#' @export
date_to_triangle_fert <- function(date, dob,
                                  break_min = 15,
                                  break_max = 50,
                                  recode_up = TRUE,
                                  recode_down = TRUE,
                                  first_month = "Jan",
                                  year_to = TRUE,
                                  as_factor = TRUE) {
    n <- length(date)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    if (as_factor) {
        levels <- c("Lower", "Upper")
        has_na <- any(is.na(date)) || any(is.na(dob))
        if (has_na)
            levels <- c(levels, NA)
        ans <- factor(ans,
                      levels = levels,
                      exclude = NULL)
    }
    ans
}

#' @rdname date_to_triangle
#' @export
date_to_triangle_multi <- function(date, dob,
                                   break_max = 100,
                                   width = 5,
                                   first_month = "Jan",
                                   as_factor = TRUE) {
    n <- length(date)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    if (as_factor) {
        levels <- c("Lower", "Upper")
        has_na <- any(is.na(date)) || any(is.na(dob))
        if (has_na)
            levels <- c(levels, NA)
        ans <- factor(ans,
                      levels = levels,
                      exclude = NULL)
    }
    ans
}

## HAS_TESTS
#' @rdname date_to_triangle
#' @export
date_to_triangle_quarter <- function(date,
                                     dob,
                                     break_max = 400,
                                     open_right = TRUE,
                                     as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (!is.null(break_max) && !open_right)
        demcheck::err_exceeds_break_max_age(age = age_months,
                                            break_max = break_max,
                                            date = date,
                                            dob = dob,
                                            unit = "month")
    n <- length(date)
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    i_month_within_qu_date <- (date_ymd$m - 1L) %% 3L
    i_month_within_qu_dob <- (dob_ymd$m - 1L) %% 3L
    is_lower_within_month <- is_lower_within_month(date_ymd = date_ymd,
                                                   dob_ymd = dob_ymd)
    is_lower <- ((i_month_within_qu_date > i_month_within_qu_dob)
        | ((i_month_within_qu_date == i_month_within_qu_dob)
            & is_lower_within_month))
    ans[is_lower] <- "Lower"
    may_have_ages_above_break_max <- !is.null(break_max) && open_right
    if (may_have_ages_above_break_max) {
        age_start <- age_completed_months_start_month(date_ymd = date_ymd,
                                                      dob_ymd = dob_ymd)
        is_open_upper <- !is.na(age_start) & (age_start >= break_max)
        ans[is_open_upper] <- "Upper"
    }
    if (as_factor) {
        levels <- c("Lower", "Upper")
        has_na <- any(is.na(date)) || any(is.na(dob))
        if (has_na)
            levels <- c(levels, NA)
        ans <- factor(ans,
                      levels = levels,
                      exclude = NULL)
    }
    ans
}

## HAS_TESTS
#' @rdname date_to_triangle
#' @export
date_to_triangle_month <- function(date, dob,
                                   break_max = 1200,
                                   open_right = TRUE,
                                   as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (!is.null(break_max) && !open_right)
        demcheck::err_exceeds_break_max_age(age = age_months,
                                            break_max = break_max,
                                            date = date,
                                            dob = dob,
                                            unit = "month")
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    n <- length(date)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    is_lower <- is_lower_within_month(date_ymd = date_ymd,
                                      dob_ymd = dob_ymd)
    ans[is_lower] <- "Lower"
    may_have_ages_above_break_max <- !is.null(break_max) && open_right
    if (may_have_ages_above_break_max) {
        age_start <- age_completed_months_start_month(date_ymd = date_ymd,
                                                      dob_ymd = dob_ymd)
        is_open_upper <- !is.na(age_start) & (age_start >= break_max)
        ans[is_open_upper] <- "Upper"
    }
    if (as_factor) {
        levels <- c("Lower", "Upper")
        has_na <- any(is.na(date)) || any(is.na(dob))
        if (has_na)
            levels <- c(levels, NA)
        ans <- factor(ans,
                      levels = levels,
                      exclude = NULL)
    }
    ans
}
