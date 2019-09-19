
## HAS_TESTS
age_completed_months <- function(date, dob) {
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    (12L * (date_ymd$y - dob_ymd$y)
        + (date_ymd$m - dob_ymd$m)
        - (date_ymd$d < dob_ymd$d))
}

## HAS_TESTS
as_ymd <- function(date) {
    if (!inherits(date, "POSIXlt"))
        date <- as.POSIXlt(date)
    y <- date$year + 1900L
    m <- date$mon + 1L
    d <- date$mday
    is_29_feb <- (m == 2L) & (d == 29L)
    d[is_29_feb] <- 28L
    list(y = y,
         m = m,
         d = d)
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
make_age_labels_month_quarter <- function(min_break,
                                          max_break,
                                          open_left,
                                          open_right,
                                          unit,
                                          include_na) {
    min_break <- demcheck::err_tdy_non_negative_integer_scalar(x = min_break,
                                                              name = "min_break")
    max_break <- demcheck::err_tdy_positive_integer_scalar(x = max_break,
                                                           name = "max_break")
    demcheck::err_is_gt_scalar(x1 = max_break,
                               x2 = min_break,
                               name1 = "max_break",
                               name2 = "min_break")
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    suffix <- switch(unit,
                     month = "m",
                     quarter = "q",
                     stop(gettextf("can't handle unit '%s'",
                                   unit)))
    s <- seq.int(from = min_break,
                 to = max_break - 1L)
    ans_mid <- sprintf("%d%s", s, suffix)
    if (open_left)
        ans_left <- paste0("<", ans_mid[[1]])
    else
        ans_left <- NULL
    if (open_right)
        ans_right <- sprintf("%d%s+", max_break, suffix)
    else
        ans_right <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_left, ans_mid, ans_right, ans_na)
    ans
}


date_ymd_ge <- function(y1, m1, d1, y2, m2, d2) {
    (y1 > y2) ||
        ((y1 == y2) && (m1 > m2)) ||
        ((y1 == y2) && (m1 == m2) && (d1 >= d2))
}


## HAS_TESTS
make_breaks_date_year <- function(date,
                                  year_min,
                                  year_max,
                                  origin,
                                  width,
                                  first_month) {
    month_origin <- match(first_month, month.abb)
    if (is.infinite(year_min)) {
        date_first <- min(date, na.rm = TRUE)
        date_first_ymd <- as_ymd(date_first)
        year_first <- date_first_ymd$y
        month_first <- date_first_ymd$m
        day_first <- date_first_ymd$d
        diff_first_origin <- diff_completed_year(y1 = year_first,
                                                 m1 = month_first,
                                                 d1 = day_first,
                                                 y2 = origin,
                                                 m2 = month_origin,
                                                 d2 = 1L)
        date_first_ge_origin <- date_ymd_ge(y1 = year_first,
                                            m1 = month_first,
                                            d1 = day_first,
                                            y2 = origin,
                                            m2 = month_origin,
                                            d2 = 1L)
        if (date_first_ge_origin)
            year_from <- origin + (diff_first_origin %/% width) * width
        else
            year_from <- origin + ((diff_first_origin - 1L) %/% width) * width
    }
    else
        year_from <- year_min
    if (is.infinite(year_max)) {
        date_last <- max(date, na.rm = TRUE)
        date_last_ymd <- as_ymd(date_last)
        year_last <- date_last_ymd$y
        month_last <- date_last_ymd$m
        day_last <- date_last_ymd$d
        diff_last_origin <- diff_completed_year(y1 = year_last,
                                                m1 = month_last,
                                                d1 = day_last,
                                                y2 = origin,
                                                m2 = month_origin,
                                                d2 = 1L)
        date_last_ge_origin <- date_ymd_ge(y1 = year_last,
                                           m1 = month_last,
                                           d1 = day_last,
                                           y2 = origin,
                                           m2 = month_origin,
                                           d2 = 1L)
        if (date_last_ge_origin)
            year_to <- origin + (diff_last_origin %/% width + 1L) * width
        else
            year_to <- origin + ((diff_last_origin - 1L) %/% width + 1L) * width
    }
    else
        year_to <- year_max
    date_from <- sprintf("%d-%d-%d", year_from, month_origin, 1L)
    date_to <- sprintf("%d-%d-%d", year_to, month_origin, 1L)
    date_from <- as.Date(date_from, format = "%Y-%m-%d")
    date_to <- as.Date(date_to, format = "%Y-%m-%d")
    by <- paste(width, "year")
    seq.Date(from = date_from,
             to = date_to,
             by = by)
}

## HAS_TESTS
make_breaks_integer_lifetab <- function(age_max) {
    c(0L,
      1L,
      seq.int(from = 5L,
              to = age_max,
              by = 5L))
}

## HAS_TESTS
make_breaks_integer_year <- function(age, width, age_max, open_right) {
    if (is.finite(age_max))
        break_max <- age_max
    else {
        break_max <- max(age,
                         na.rm = TRUE)
        if (open_right)
            break_max <- (break_max %/% width) * width
        else
            break_max <- (break_max %/% width + 1L) * width
    }
    breaks <- seq.int(from = 0L,
                      to = break_max,
                      by = width)
}






## possible ------------------------------------------------------------


is_lower_within_month <- function(date_ymd, dob_ymd) {
    ((date_ymd$d - 1L) %/% 2L) >= (dob_ymd$d %/% 2L)
}

age_completed_months_start_month <- function(date_ymd, dob_ymd) {
    (12L * (date_ymd$y - dob_ymd$y)
        + (date_ymd$m - dob_ymd$m)
        - (dob_ymd$d != 1L))
}


    
    




