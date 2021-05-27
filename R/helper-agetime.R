
## Functions to help with age-time calculations that end users do not use directly.

## HAS_TESTS
## Find the first date 'd' such that
##   (i) if 'n' is negative - a person who is born on 'd'
##       is 'n' months old on date 'date', or
##   (ii) if 'n' is positive - a person who is born
##       on date 'date' is 'n' months old on date 'd'.
## 'date' should have class "Date" and 'n'
## should have class "integer".
## 'date' and 'n' can both have NAs.
add_months <- function(date, n) {
    date_ymd <- as_ymd(date)
    n_month_total <- 12L * (date_ymd$y - 1L) + (date_ymd$m - 1L) + n ## 0-based
    y_ans <- (n_month_total %/% 12L) + 1L ## 1-based
    m_ans <- (n_month_total %% 12L) + 1L ## 1-based
    d_ans <- date_ymd$d
    date_start_month <- ifelse(is.na(n_month_total),
                               NA_character_,
                               sprintf("%d-%d-01",
                                       y_ans,
                                       m_ans))
    date_start_month <- as.Date(date_start_month)
    n_day_month <- n_day_month(date_start_month)
    need_to_roll <- !is.na(d_ans) & !is.na(n_day_month) & (d_ans > n_day_month)
    roll_back <- need_to_roll & (n < 0L)
    roll_forward <- need_to_roll & (n > 0L)
    d_ans[roll_back] <- n_day_month[roll_back]
    d_ans[roll_forward] <- 1L
    ## should never roll forward from December,
    ## since December has 31 days, so OK to
    ## add on one month without adjusting year
    m_ans[roll_forward] <- m_ans + 1L
    ans <- ifelse(is.na(y_ans) | is.na(m_ans) | is.na(d_ans),
                  NA_character_,
                  sprintf("%d-%d-%d",
                          y_ans,
                          m_ans,
                          d_ans))
    as.Date(ans)
}

## HAS_TESTS
## Add 'n' quarters to 'date'.
## 'date' should have class "Date" and 'n'
## should have class "integer"
## 'date' and 'n' can both have NAs
add_quarters <- function(date, n) {
    n <- 3L * n
    add_months(date = date,
               n = n)
}

## HAS_TESTS
## Find the first date 'd' such that
##   (i) if 'n' is negative - a person who is born on 'd'
##       is 'n' years old on date 'date', or
##   (ii) if 'n' is positive - a person who is born
##       on date 'date' is 'n' years old on date 'd'.
## This means replacing 'y-m-d' with
## '(y+n)-m-d' except when 'date' is
## 'y-02-29' and y+n is not a leap year. In this case
##   (i) if 'n' is negative - answer is '(y+n)-02-28'
##   (ii) if 'n' is positive - answer is '(y+n)-03-01'.
## 'date' should have class "Date" and 'n'
## should have class "integer"
## 'date' and 'n' can both have NAs
add_years <- function(date, n) {
    date_ymd <- as_ymd(date)
    y_date <- date_ymd$y
    m_date <- date_ymd$m
    d_date <- date_ymd$d
    date_is_29_feb <- (m_date == 2) & (d_date == 29L)
    y_ans <- y_date + n
    m_ans <- m_date
    d_ans <- d_date
    y_ans_is_leap_year <- is_leap_year(y_ans)
    need_to_roll <- (!is.na(date_is_29_feb)
        & !is.na(y_ans_is_leap_year)
        & date_is_29_feb 
        & !y_ans_is_leap_year)
    roll_back <- need_to_roll & (n < 0L)
    roll_forward <- need_to_roll & (n > 0L)
    d_ans[roll_back] <- 28L
    d_ans[roll_forward] <- 1L
    m_ans[roll_forward] <- 3L
    ans <- ifelse(is.na(y_ans) | is.na(m_ans) | is.na(d_ans),
                  NA_character_,
                  sprintf("%d-%d-%d",
                          y_ans,
                          m_ans,
                          d_ans))
    as.Date(ans)
}

## HAS_TESTS
## Assume that 'date' and 'dob' are valid.
age_completed_months <- function(date, dob) {
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    (12L * (date_ymd$y - dob_ymd$y)
        + (date_ymd$m - dob_ymd$m)
        - (date_ymd$d < dob_ymd$d))
}

## HAS_TESTS
## Assume that 'date_ymd' and 'dob_ymd' are valid.
age_completed_months_start_month <- function(date_ymd, dob_ymd) {
    (12L * (date_ymd$y - dob_ymd$y)
        + (date_ymd$m - dob_ymd$m)
        - (dob_ymd$d != 1L))
}

## HAS_TESTS
## Assume that 'date' and 'dob' are valid.
age_completed_years <- function(date, dob) {
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    passed_month <- date_ymd$m > dob_ymd$m
    reached_month <- date_ymd$m == dob_ymd$m
    reached_day <- date_ymd$d >= dob_ymd$d
    reached_birthday <- passed_month | (reached_month & reached_day)
    date_ymd$y - dob_ymd$y - !reached_birthday
}

## HAS_TESTS
as_ymd <- function(date) {
    if (!inherits(date, "POSIXlt"))
        date <- as.POSIXlt(date)
    y <- date$year + 1900L
    m <- date$mon + 1L
    d <- date$mday
    list(y = y,
         m = m,
         d = d)
}

## HAS_TESTS
## returns the date equivalent of -Inf if
## an interval is open on the left
date_start_month <- function(labels) {
    is_na <- is.na(labels)
    ans <- rep(as.Date(NA), times = length(labels))
    labels_obs <- labels[!is_na]
    labels_obs <- sub("^<|\\+$", "", labels_obs)
    date_obs <- paste(labels_obs, "1")
    date_obs <- as.Date(date_obs, format = "%Y %b %d")
    ans[!is_na] <- date_obs
    ans
}

## HAS_TESTS
## If interval is open on the left, return the
## upper of limit of the interval
date_start_quarter <- function(labels) {
    ans <- sub("^<|\\+$", "", labels)
    ans <- sub(" Q1$", "-01-01", ans)
    ans <- sub(" Q2$", "-04-01", ans)
    ans <- sub(" Q3$", "-07-01", ans)
    ans <- sub(" Q4$", "-10-01", ans)
    ans <- as.Date(ans)
    ans
}



## HAS_TESTS
date_ymd_ge <- function(y1, m1, d1, y2, m2, d2) {
    (y1 > y2) ||
        ((y1 == y2) && (m1 > m2)) ||
        ((y1 == y2) && (m1 == m2) && (d1 >= d2))
}

## HAS_TESTS
diff_completed_year <- function(y1, m1, d1, y2, m2, d2) {
    same_day <- (m1 == m2) && (d1 == d2)
    if (same_day)
        return(y1 - y2)
    day1_gt_day2 <- (m1 > m2) || ((m1 == m2) && (d1 > d2))
    if (day1_gt_day2) {
        if (y1 >= y2)
            y1 - y2
        else
            y1 - y2 + 1L
    }
    else {
        if (y1 > y2)
            y1 - y2 - 1L
        else
            y1 - y2
    }
}

## HAS_TESTS
is_leap_year <- function(year) {
    div_by_4 <- (year %% 4L) == 0L
    div_by_100 <- (year %% 100L) == 0L
    div_by_400 <- (year %% 400L) == 0L
    div_by_400 | (div_by_4 & !div_by_100)
}
        
## HAS_TESTS
is_lower_within_month <- function(date_ymd, dob_ymd) {
    ((date_ymd$d - 1L) %/% 2L) >= (dob_ymd$d %/% 2L)
}

## NO_TESTS
make_breaks_date_to_date_month <- function(date) {
    ## break_min
    date_min <- min(date, na.rm = TRUE)
    date_min_ymd <- as_ymd(date_min)
    year_min <- date_min_ymd$y
    month_min <- date_min_ymd$m
    break_min <- paste(year_min, month_min, 1, sep = "-")
    break_min <- as.Date(break_min)
    ## break_max
    date_max <- max(date, na.rm = TRUE)
    date_max_ymd <- as_ymd(date_max)
    year_max <- date_max_ymd$y
    month_max <- date_max_ymd$m
    month_max <- month_max + 1L
    if (month_max > 12L) {
        year_max <- year_max + 1L
        month_max <- 1L
    }
    break_max <- paste(year_max, month_max, 1, sep = "-")
    break_max <- as.Date(break_max)
    ## sequence
    seq.Date(from = break_min,
             to = break_max,
             by = "month")
}

## NO_TESTS
make_breaks_date_to_date_quarter <- function(date) {
    ## break_min
    date_min <- min(date, na.rm = TRUE)
    date_min_ymd <- as_ymd(date_min)
    year_min <- date_min_ymd$y
    month_min <- date_min_ymd$m
    month_min <- ((month_min - 1L) %/% 3L) * 3L + 1L
    break_min <- paste(year_min, month_min, 1, sep = "-")
    break_min <- as.Date(break_min)
    ## break_max
    date_max <- max(date, na.rm = TRUE)
    date_max_ymd <- as_ymd(date_max)
    year_max <- date_max_ymd$y
    month_max <- date_max_ymd$m
    month_max <- ((month_max - 1L) %/% 3L + 1L) * 3L + 1L
    if (month_max > 12L) {
        year_max <- year_max + 1L
        month_max <- 1L
    }
    break_max <- paste(year_max, month_max, 1, sep = "-")
    break_max <- as.Date(break_max)
    ## sequence
    seq.Date(from = break_min,
             to = break_max,
             by = "quarter")
}

## NO_TESTS
make_breaks_date_to_date_year <- function(date,
                                          month_start) {
    month_start_int <- match(month_start, base::month.abb)
    ## obtain 'break_min'
    date_min <- min(date, na.rm = TRUE)
    date_min_ymd <- as_ymd(date_min)
    year_min <- date_min_ymd$y
    month_min <- date_min_ymd$m
    if (month_min >= month_start_int)
        break_min <- paste(year_min, month_start_int, 1L, sep = "-")
    else
        break_min <- paste(year_min - 1L, month_start_int, 1L, sep = "-")
    break_min <- as.Date(break_min)
    ## obtain 'break_max'
    date_max <- max(date, na.rm = TRUE)
    date_max_ymd <- as_ymd(date_max)
    year_max <- date_max_ymd$y
    month_max <- date_max_ymd$m
    if (month_start_int > month_max)
        break_max <- paste(year_max, month_start_int, 1L, sep = "-")
    else
        break_max <- paste(year_max + 1L, month_start_int, 1L, sep = "-")
    break_max <- as.Date(break_max)
    ## return result
    seq.Date(from = break_min,
             to = break_max,
             by = "year")
}

## HAS_TESTS
make_breaks_date_to_integer_births <- function(age,
                                               width,
                                               break_min,
                                               break_max) {
    if (is.null(break_min)) {
        break_min <- min(age, na.rm = TRUE)
        break_min <- (break_min %/% width) * width
        message(gettextf("'%s' set to %d",
                         "break_min", break_min),
                appendLF = TRUE)
    }
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE)
        break_max <- (break_max %/% width + 1L) * width
        message(gettextf("'%s' set to %d",
                         "break_max", break_max),
                appendLF = TRUE)
    }
    seq.int(from = break_min,
            to = break_max,
            by = width)
}

## HAS_TESTS
make_breaks_date_to_integer_lifetab <- function(age,
                                                break_max) {
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE)
        break_max <- (break_max %/% 5L + 1L) * 5L
        message(gettextf("'%s' set to %d",
                         "break_max", break_max),
                appendLF = TRUE)
    }
    c(0L,
      1L,
      seq.int(from = 5L,
              to = break_max,
              by = 5L))
}

## HAS_TESTS
make_breaks_date_to_integer_month_quarter <- function(age,
                                                      break_min,
                                                      break_max,
                                                      has_break_min_arg,
                                                      has_break_max_arg) {
    if (is.null(break_min)) {
        break_min <- min(age, na.rm = TRUE)
        if (has_break_min_arg) {
            message(gettextf("'%s' set to %d",
                             "break_min", break_min),
                    appendLF = TRUE)
        }
    }
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE) + 1L
        if (has_break_max_arg) {
            message(gettextf("'%s' set to %d",
                             "break_max", break_max),
                    appendLF = TRUE)
        }
    }
    seq.int(from = break_min,
            to = break_max)
}

## HAS_TESTS
make_breaks_date_to_integer_year <- function(age,
                                             width,
                                             break_min,
                                             break_max,
                                             has_break_min_arg,
                                             has_break_max_arg) {
    if (is.null(break_min)) {
        break_min <- min(age, na.rm = TRUE)
        break_min <- (break_min %/% width) * width
        if (has_break_min_arg) {
            message(gettextf("'%s' set to %d",
                             "break_min", break_min),
                    appendLF = TRUE)
        }
    }
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE)
        break_max <- (break_max %/% width + 1L) * width
        if (has_break_max_arg) {
            message(gettextf("'%s' set to %d",
                             "break_max", break_max),
                    appendLF = TRUE)
        }
    }
    seq.int(from = break_min,
            to = break_max,
            by = width)
}

## HAS_TESTS
## In places where 'is_open' is TRUE, the intervals
## are open on the left. 'unit' must be "month"
## or "quarter"
make_breaks_label_to_date_month_quarter <- function(date_low,
                                                    date_up,
                                                    break_min,
                                                    has_break_min_arg,
                                                    is_open,
                                                    unit) {
    ## determine 'break_min'
    if (is.null(break_min)) {
        if (any(is_open))
            break_min <- max(date_up[is_open])
        else
            break_min <- min(date_low, na.rm = TRUE)
        if (has_break_min_arg) {
            message(gettextf("'%s' set to \"%s\"",
                             "break_min", break_min),
                    appendLF = TRUE)
        }
    }
    ## Determine 'break_max'. 
    break_max <- max(date_up, na.rm = TRUE)
    ## make breaks
    breaks <- seq.Date(from = break_min,
                       to = break_max,
                       by = unit)
    ## return value
    breaks
}

## HAS_TESTS
make_breaks_label_to_integer_births <- function(age_low,
                                                age_up,
                                                labels,
                                                width,
                                                break_min,
                                                break_max) {
    if (is.null(break_min)) {
        break_min <- min(age_low, na.rm = TRUE)
        break_min <- (break_min %/% width) * width
        message(gettextf("'%s' set to %d",
                         "break_min", break_min),
                appendLF = TRUE)
    }
    if (is.null(break_max)) {
        break_max <- max(age_up, na.rm = TRUE)
        remainder <- break_max %% width
        if (remainder > 0L)
            break_max <- break_max + width - remainder
        message(gettextf("'%s' set to %d",
                         "break_max", break_max),
                appendLF = TRUE)
    }
    breaks <- seq.int(from = break_min,
                      to = break_max,
                      by = width)
    demcheck::err_intervals_inside_breaks(int_low = age_low,
                                          int_up = age_up,
                                          breaks = breaks,
                                          labels = labels)
    breaks
}

## HAS_TESTS
make_breaks_label_to_integer_lifetab <- function(age_low,
                                                 age_up,
                                                 labels,
                                                 is_open,
                                                 break_max) {
    if (is.null(break_max)) {
        break_max <- max(age_low[is_open], na.rm = TRUE)
        remainder <- break_max %% 5L
        if (remainder > 0L)
            break_max <- break_max + 5L - remainder
        message(gettextf("'%s' set to %d",
                         "break_max", break_max),
                appendLF = TRUE)
    }
    breaks <- c(0L,
                1L,
                seq.int(from = 5L,
                        to = break_max,
                        by = 5L))
    demcheck::err_intervals_inside_breaks(int_low = age_low,
                                          int_up = age_up,
                                          breaks = breaks,
                                          labels = labels)
    breaks
}

## HAS_TESTS
make_breaks_label_to_integer <- function(int_low,
                                         int_up,
                                         labels,
                                         width,
                                         origin,
                                         break_min,
                                         break_max,
                                         has_break_min_arg,
                                         has_break_max_arg) {
    ## classify intervals
    is_na_low <- is.na(int_low)
    is_na_up <- is.na(int_up)
    is_na <- is_na_low & is_na_up
    is_open_low <- is_na_low & !is_na_up
    is_open_up <- !is_na_low & is_na_up
    open_first <- any(is_open_low)
    open_last <- any(is_open_up)
    ## determine 'break_min'
    if (is.null(break_min)) {
        if (open_first)
            break_min <- max(int_up[is_open_low])
        else
            break_min <- min(int_low, na.rm = TRUE)
        break_min <- break_min - origin
        if (open_first) {
            remainder <- break_min %% width
            if (remainder > 0L)
                break_min <- break_min + width - remainder
        }
        else
            break_min <- (break_min %/% width) * width
        break_min <- break_min + origin
        if (has_break_min_arg) {
            message(gettextf("'%s' set to %d",
                             "break_min", break_min),
                    appendLF = TRUE)
        }
    }
    ## determine 'break_max'
    if (is.null(break_max)) {
        if (open_last)
            break_max <- min(int_low[is_open_up])
        else
            break_max <- max(int_up, na.rm = TRUE)
        break_max <- break_max - origin
        if (open_last)
            break_max <- (break_max %/% width) * width
        else {
            remainder <- break_max %% width
            if (remainder > 0L)
                break_max <- break_max + width - remainder
        }
        break_max <- break_max + origin
        if (has_break_max_arg) {
            message(gettextf("'%s' set to %d",
                             "break_max", break_max),
                    appendLF = TRUE)
        }
    }
    ## make breaks
    breaks <- seq.int(from = break_min,
                      to = break_max,
                      by = width)
    ## check that no intervals cross breaks
    demcheck::err_intervals_inside_breaks(int_low = int_low,
                                          int_up = int_up,
                                          breaks = breaks,
                                          labels = labels)
    ## return value
    breaks
}


## HAS_TESTS
## Number of days in month containing 'date'. Handles leap years.
n_day_month <- function(date) {
    n_day <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    date_ymd <- as_ymd(date)
    year <- date_ymd$y
    month <- date_ymd$m
    is_leap_year <- is_leap_year(year)
    is_feb <- month == 2L
    is_feb_in_leap_year <- is_leap_year & is_feb
    ans <- n_day[month]
    ans[is_feb_in_leap_year] <- 29L
    ans
}

## HAS_TESTS
## Roll back to first day of current month.
rollback_month <- function(date) {
    if (identical(length(date), 0L))
        return(as.Date(character()))
    date <- as.POSIXlt(date)
    date$mday <- 1L
    as.Date(date)
}

## HAS_TESTS
## Roll back to first day of current quarter.
rollback_quarter <- function(date) {
    if (identical(length(date), 0L))
        return(as.Date(character()))
    date <- as.POSIXlt(date)
    date$mday <- 1L
    date$mon <- (date$mon %/% 3L) * 3L
    as.Date(date)
}

## HAS_TESTS
## Roll back to first day of current year.
rollback_year <- function(date, month_start) {
    if (identical(length(date), 0L))
        return(as.Date(character()))
    date <- as.POSIXlt(date)
    i_month_start <- match(month_start, base::month.abb) - 1L
    is_before_month_start <- !is.na(date) & (date$mon < i_month_start)
    date$year[is_before_month_start] <- date$year[is_before_month_start] - 1L
    date$mon <- i_month_start
    date$mday <- 1L
    as.Date(date)
}


## HAS_TESTS
## Roll forward to first day of next month.
rollforward_month <- function(date) {
    if (identical(length(date), 0L))
        return(as.Date(character()))
    date <- as.POSIXlt(date)
    date$mday <- 1L
    date$mon <- date$mon + 1L
    as.Date(date)
}


## NO_TESTS
## Roll forward to first day of next quarter.
rollforward_quarter <- function(date) {
    if (identical(length(date), 0L))
        return(as.Date(character()))
    date <- as.POSIXlt(date)
    date$mday <- 1L
    mon_old <- date$mon
    remainder <- mon_old %% 3L
    mon_new <- mon_old + 3L - remainder
    date$mon <- mon_new
    as.Date(date)
}

