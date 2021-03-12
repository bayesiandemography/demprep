
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
i_month_within_period <- function(date_ymd, width, origin, month_start) {
    year <- date_ymd$y
    month <- date_ymd$m
    i_month_start <- match(month_start, month.abb) # starts at 1
    i_year <- (year - origin) %% width # starts at 0
    i_month_within_year <- month - i_month_start  # starts at 0
    ans <- 12L * i_year + i_month_within_year + 1L # starts at 1
    is_neg <- !is.na(month) & (i_month_within_year < 0L)
    ans[is_neg] <- ans[is_neg] + 12L * width
    ans
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

## HAS_TESTS
make_breaks_date_to_date_month <- function(date, break_min) {
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    ## date_from
    if (has_break_min)
        date_from <- break_min
    else {
        date_first <- min(date, na.rm = TRUE)
        date_first_ymd <- as_ymd(date_first)
        year_first <- date_first_ymd$y
        month_first <- date_first_ymd$m
        date_from <- sprintf("%d-%d-01", year_first, month_first)
        date_from <- as.Date(date_from)
        message(gettextf("'%s' set to %s",
                         "break_min", date_from),
                appendLF = TRUE)
    }
    ## date_to
    if (has_date) {
        date_last <- max(date, na.rm = TRUE)
        date_last_ymd <- as_ymd(date_last)
        year_last <- date_last_ymd$y
        month_last <- date_last_ymd$m
        year_to <- year_last
        month_to <- month_last + 1L
        if (month_to > 12L) {
            year_to <- year_to + 1L
            month_to <- 1L
        }
        date_to <- sprintf("%d-%d-01", year_to, month_to)
        date_to <- as.Date(date_to)
    }
    else
        date_to <- break_min
    ## sequence
    seq.Date(from = date_from,
             to = date_to,
             by = "month")
}

## HAS_TESTS
make_breaks_date_to_date_quarter <- function(date, break_min) {
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    ## date_from
    if (has_break_min)
        date_from <- break_min
    else {
        date_first <- min(date, na.rm = TRUE)
        date_first_ymd <- as_ymd(date_first)
        year_first <- date_first_ymd$y
        month_first <- date_first_ymd$m
        year_from <- year_first
        month_from <- ((month_first - 1L) %/% 3L) * 3L + 1L
        date_from <- sprintf("%d-%d-01", year_from, month_from)
        date_from <- as.Date(date_from)
        message(gettextf("'%s' set to %s",
                         "break_min", date_from),
                appendLF = TRUE)
    }
    ## date_to
    if (has_date) {
        date_last <- max(date, na.rm = TRUE)
        date_last_ymd <- as_ymd(date_last)
        year_last <- date_last_ymd$y
        month_last <- date_last_ymd$m
        year_to <- year_last
        month_to <- ((month_last - 1L) %/% 3L + 1L) * 3L + 1L
        if (month_to > 12L) {
            year_to <- year_to + 1L
            month_to <- 1L
        }
        date_to <- sprintf("%d-%d-01", year_to, month_to)
        date_to <- as.Date(date_to)
    }
    else
        date_to <- break_min
    ## sequence
    seq.Date(from = date_from,
             to = date_to,
             by = "quarter")
}

## HAS_TESTS
make_breaks_date_to_date_year <- function(date,
                                          month_start,
                                          width,
                                          origin,
                                          break_min) {
    has_date <- sum(!is.na(date)) > 0L
    has_break_min <- !is.null(break_min)
    has_origin <- !is.null(origin)
    ## get year of 'break_min'
    if (has_break_min) {
        year_break_min <- format(break_min, "%Y")
        year_break_min <- as.integer(year_break_min)
    }
    ## obtain 'year_origin', 'month_origin', 'day_origin'
    if (has_break_min)
        origin <- year_break_min
    else {
        if (!has_origin) {
            if (!identical(width, 1L))
                stop(gettextf("'%s' is %s but '%s' equals %d",
                              "origin", "NULL", "width", width))
            origin <- 2000L
        }
    }
    year_origin <- origin
    month_origin <- match(month_start, month.abb)
    day_origin <- 1L
    ## obtain 'year_from'
    if (has_break_min)
        year_from <- year_break_min
    else {
        if (has_date) {
            date_first <- min(date, na.rm = TRUE)
            date_first_ymd <- as_ymd(date_first)
            year_first <- date_first_ymd$y
            month_first <- date_first_ymd$m
            day_first <- date_first_ymd$d
            diff_first_origin <- diff_completed_year(y1 = year_first,
                                                     m1 = month_first,
                                                     d1 = day_first,
                                                     y2 = year_origin,
                                                     m2 = month_origin,
                                                     d2 = day_origin)
            date_first_ge_origin <- date_ymd_ge(y1 = year_first,
                                                m1 = month_first,
                                                d1 = day_first,
                                                y2 = year_origin,
                                                m2 = month_origin,
                                                d2 = 1L)
            if (date_first_ge_origin)
                year_from <- year_origin + (diff_first_origin %/% width) * width
            else {
                same_day <- (month_first == month_origin) && (day_first == 1L)
                year_from <- year_origin + ((diff_first_origin - 1L + same_day) %/% width) * width
            }
        }
        else
            year_from <- year_origin
    }
    ## obtain 'year_to'
    if (has_date) {
        date_last <- max(date, na.rm = TRUE)
        date_last_ymd <- as_ymd(date_last)
        year_last <- date_last_ymd$y
        month_last <- date_last_ymd$m
        day_last <- date_last_ymd$d
        diff_last_origin <- diff_completed_year(y1 = year_last,
                                                m1 = month_last,
                                                d1 = day_last,
                                                y2 = year_origin,
                                                m2 = month_origin,
                                                d2 = day_origin)
        date_last_ge_origin <- date_ymd_ge(y1 = year_last,
                                           m1 = month_last,
                                           d1 = day_last,
                                           y2 = year_origin,
                                           m2 = month_origin,
                                           d2 = day_origin)
        if (date_last_ge_origin)
            year_to <- year_origin + (diff_last_origin %/% width + 1L) * width
        else
            year_to <- year_origin + ((diff_last_origin - 1L) %/% width + 1L) * width
    }
    else
        year_to <- year_from
    ## create series
    date_from <- sprintf("%d-%d-%d", year_from, month_origin, day_origin)
    date_to <- sprintf("%d-%d-%d", year_to, month_origin, day_origin)
    date_from <- as.Date(date_from, format = "%Y-%m-%d")
    date_to <- as.Date(date_to, format = "%Y-%m-%d")
    by <- paste(width, "year")
    if (!has_break_min) {
        message(gettextf("'%s' set to %s",
                         "break_min", date_from),
                appendLF = TRUE)
    }
    seq.Date(from = date_from,
             to = date_to,
             by = by)
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
make_breaks_date_to_integer_month_quarter <- function(age, break_min, break_max, open_last) {
    if (is.null(break_min)) {
        break_min <- min(age, na.rm = TRUE)
        message(gettextf("'%s' set to %d",
                         "break_min", break_min),
                appendLF = TRUE)
    }
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE)
        if (!open_last)
            break_max <- break_max + 1L
        message(gettextf("'%s' set to %d",
                         "break_max", break_max),
                appendLF = TRUE)
    }
    seq.int(from = break_min,
            to = break_max)
}

## HAS_TESTS
make_breaks_date_to_integer_year <- function(age, width, break_min,
                                             break_max, open_last) {
    if (is.null(break_min)) {
        break_min <- min(age, na.rm = TRUE)
        break_min <- (break_min %/% width) * width
        message(gettextf("'%s' set to %d",
                         "break_min", break_min),
                appendLF = TRUE)
    }
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE)
        if (open_last)
            break_max <- (break_max %/% width) * width
        else
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
make_breaks_label_to_integer_lifetab <- function(age_low,
                                                 is_open,
                                                 break_max) {
    if (is.null(break_max)) {
        break_max <- max(age_low[is_open], na.rm = TRUE)
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
make_breaks_label_to_integer_year <- function(age_low,
                                              age_up,
                                              labels,
                                              width,
                                              is_open,
                                              break_min,
                                              break_max,
                                              open_last) {
    ## determine 'break_min'
    if (is.null(break_min)) {
        break_min <- min(age_low, na.rm = TRUE)
        break_min <- (break_min %/% width) * width
        message(gettextf("'%s' set to %d",
                         "break_min", break_min),
                appendLF = TRUE)
    }
    ## Determine 'break_max'. Formula different from 'date_to_integer'
    ## equivalent, since age in that case is age in completed years,
    ## whereas here it is the upper limit of the age group.
    if (is.null(break_max)) {
        if (any(is_open))
            break_max <- min(age_low[is_open])
        else
            break_max <- max(age_up, na.rm = TRUE)
        if (open_last)
            break_max <- (break_max %/% width) * width
        else {
            remainder <- break_max %% width
            if (remainder > 0L)
                break_max <- break_max + width - remainder
        }
        message(gettextf("'%s' set to %d",
                         "break_max", break_max),
                appendLF = TRUE)
    }
    ## make breaks
    breaks <- seq.int(from = break_min,
                      to = break_max,
                      by = width)
    ## check that no intervals cross breaks
    up_is_break <- age_up %in% breaks
    i_int_low <- findInterval(age_low, breaks)
    i_int_up <- findInterval(age_up, breaks)
    is_valid <- (is.na(age_low)
        | is.na(age_up)
        | (!up_is_break & (i_int_up == i_int_low))
        | (up_is_break & (i_int_up == i_int_low + 1L)))
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("age group \"%s\" intersects two or more of the intervals formed when '%s' is %d, '%s' is %d, and '%s' is %d",
                      labels[[i_invalid]],
                      "break_min",
                      break_min,
                      "break_max",
                      break_max,
                      "width",
                      width),
             call. = FALSE)
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
## Roll back to first day of current multi-year period.
rollback_multi <- function(date, width, origin, month_start) {
    if (identical(length(date), 0L))
        return(as.Date(character()))
    if (all(is.na(date)))
        return(date)
    breaks <- make_breaks_date_to_date_year(date = date,
                                    month_start = month_start,
                                    width = width,
                                    origin = origin,
                                    break_min = NULL)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    breaks[i]
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
## Roll forward to first day of next month.
rollforward_month <- function(date) {
    if (identical(length(date), 0L))
        return(as.Date(character()))
    date <- as.POSIXlt(date)
    date$mday <- 1L
    date$mon <- date$mon + 1L
    as.Date(date)
}
    
