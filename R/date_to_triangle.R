#' Use dates and dates of birth to calculate Lexis triangles
#' 
#' @name date_to_triangle
NULL

## HAS_TESTS
#' @rdname date_to_triangle
#' @export
date_to_triangle_year <- function(date,
                                  dob,
                                  break_max = 100,
                                  open_right = TRUE,
                                  first_month = "Jan",
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
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- age_completed_months(date = date,
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
    i_month_within_yr_date <- i_month_within_period(date = date_ymd,
                                                    width = 1L,
                                                    origin = 2000L,
                                                    first_month = first_month)
    i_month_within_yr_dob <- i_month_within_period(date = dob_ymd,
                                                   width = 1L,
                                                   origin = 2000L,
                                                   first_month = first_month)
    is_lower_within_month <- is_lower_within_month(date_ymd = date_ymd,
                                                   dob_ymd = dob_ymd)
    is_lower <- ((i_month_within_yr_date > i_month_within_yr_dob)
        | ((i_month_within_yr_date == i_month_within_yr_dob) & is_lower_within_month))
    ans[is_lower] <- "Lower"
    may_have_ages_above_break_max <- !is.null(break_max) && open_right
    if (may_have_ages_above_break_max) {
        age_start <- age_completed_months_start_month(date_ymd = date_ymd,
                                                      dob_ymd = dob_ymd)
        is_open_upper <- !is.na(age_start) & (age_start >= 12L * break_max)
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
                                  width = 5,
                                  recode_up = FALSE,
                                  recode_down = FALSE,
                                  first_month = "Jan",
                                  as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    break_min <- demcheck::err_tdy_positive_integer_scalar(x = break_min,
                                                           name = "break_min",
                                                           null_ok = FALSE)
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = FALSE)
    demcheck::err_is_gt_scalar(x1 = break_max,
                               x2 = break_min,
                               name1 = "break_max",
                               name2 = "break_min")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if ((break_max - break_min) %% width != 0L)
        stop(gettextf("difference between '%s' [%d] and '%s' [%d] not divisible by '%s' [%d]",
                      "break_max", break_max, "break_min", break_min, "width", width))
    demcheck::err_is_logical_flag(x = recode_up,
                                  name = "recode_up")
    demcheck::err_is_logical_flag(x = recode_down,
                                  name = "recode_down")
    first_month <- demcheck::err_tdy_first_month(x = first_month,
                                                 name = "first_month")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    is_lt_min <- age_years < break_min
    i_lt_min <- match(TRUE, is_lt_min, nomatch = 0L)
    if (i_lt_min > 0L) {
        if (recode_up)
            age_years[is_lt_min] <- break_min
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
        if (recode_down)
            age_years[is_ge_max] <- break_max - 1L
        else {
            stop(gettextf(paste("'date' of \"%s\" and 'dob' of \"%s\" imply age of %d,",
                                "but 'break_max' is %d and 'recode_down' is FALSE"),
                          date[[i_ge_max]],
                          dob[[i_ge_max]],
                          age_years[[i_ge_max]],
                          break_max))
        }
    }

    ## written to here

    n <- length(date)
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    i_first_month <- match(first_month, month.abb)
    month_date <- date_ymd$m
    month_dob <- dob_ymd$m
    i_month_within_yr_date <- month_date - i_first_month 
    i_month_within_yr_dob <- month_dob - i_first_month
    is_neg_date <- !is.na(date) & i_month_within_yr_date < 0L
    is_neg_dob <- !is.na(dob) & i_month_within_yr_dob < 0L
    i_month_within_yr_date[is_neg_date] <- i_month_within_yr_date[is_neg_date] + 12L
    i_month_within_yr_dob[is_neg_dob] <- i_month_within_yr_dob[is_neg_dob] + 12L
    is_lower_within_month <- is_lower_within_month(date_ymd = date_ymd,
                                                   dob_ymd = dob_ymd)
    is_lower <- ((i_month_within_yr_date > i_month_within_yr_dob)
        | ((i_month_within_yr_date == i_month_within_yr_dob) & is_lower_within_month))



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
date_to_triangle_multi <- function(date,
                                   dob,
                                   width = 5,
                                   break_max = 100,
                                   open_right = TRUE,
                                   origin = 2000,
                                   first_month = "Jan",
                                   as_factor = TRUE) {
    l <- demcheck::err_tdy_date_dob(date = date,
                                    dob = dob)
    date <- l$date
    dob <- l$dob
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                           name = "break_max",
                                                           null_ok = TRUE)
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                               name = "origin")
    first_month <- demcheck::err_tdy_first_month(x = first_month,
                                                 name = "first_month")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    age_months <- age_completed_months(date = date,
                                       dob = dob)
    age_years <- age_months %/% 12L
    if (!open_right)
        demcheck::err_lt_break_max_age(age = age_years,
                                       break_max = break_max,
                                       date = date,
                                       dob = dob,
                                       unit = "year")
    n <- length(date)
    ans <- rep.int("Upper", times = n)
    ans[is.na(date) | is.na(dob)] <- NA_character_
    i_month_within_period_date <- i_month_within_period(date = date_ymd,
                                                        width = width,
                                                        origin = origin,
                                                        first_month = first_month)
    i_month_within_period_dob <- i_month_within_period(date = dob_ymd,
                                                       width = width,
                                                       origin = origin,
                                                       first_month = first_month)
    is_lower_within_month <- is_lower_within_month(date_ymd = date_ymd,
                                                   dob_ymd = dob_ymd)
    is_lower <- ((i_month_within_period_date > i_month_within_period_dob)
        | ((i_month_within_period_date == i_month_within_period_dob)
            & is_lower_within_month))
    ans[is_lower] <- "Lower"
    may_have_ages_above_break_max <- !is.null(break_max) && open_right
    if (may_have_ages_above_break_max) {
        age_start <- age_completed_months_start_month(date_ymd = date_ymd,
                                                      dob_ymd = dob_ymd)
        is_open_upper <- !is.na(age_start) & (age_start >= 12L * break_max)
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
        is_open_upper <- !is.na(age_start) & (age_start >= 3L * break_max)
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
