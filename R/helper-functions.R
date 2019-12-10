
## HAS_TESTS
age_completed_months <- function(date, dob) {
    date_ymd <- as_ymd(date)
    dob_ymd <- as_ymd(dob)
    (12L * (date_ymd$y - dob_ymd$y)
        + (date_ymd$m - dob_ymd$m)
        - (date_ymd$d < dob_ymd$d))
}

## HAS_TESTS
age_completed_months_start_month <- function(date_ymd, dob_ymd) {
    (12L * (date_ymd$y - dob_ymd$y)
        + (date_ymd$m - dob_ymd$m)
        - (dob_ymd$d != 1L))
}

## HAS_TESTS
as_ymd <- function(date) {
    if (!inherits(date, "POSIXlt"))
        date <- as.POSIXlt(date)
    y <- date$year + 1900L
    m <- date$mon + 1L
    d <- date$mday
    is_29_feb <- !is.na(m) & (m == 2L) & (d == 29L)
    d[is_29_feb] <- 28L
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
is_lower_within_month <- function(date_ymd, dob_ymd) {
    ((date_ymd$d - 1L) %/% 2L) >= (dob_ymd$d %/% 2L)
}

## HAS_TESTS
make_breaks_date_month <- function(date, break_min) {
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
make_breaks_date_quarter <- function(date, break_min) {
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
make_breaks_date_year <- function(date,
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
    seq.Date(from = date_from,
             to = date_to,
             by = by)
}

## HAS_TESTS
make_breaks_integer_fert <- function(age, width, break_min, break_max) {
    if (is.null(break_min)) {
        break_min <- min(age, na.rm = TRUE)
        break_min <- (break_min %/% width) * width
    }
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE)
        break_max <- (break_max %/% width + 1L) * width
    }
    seq.int(from = break_min,
            to = break_max,
            by = width)
}

## HAS_TESTS
make_breaks_integer_lifetab <- function(break_max) {
    c(0L,
      1L,
      seq.int(from = 5L,
              to = break_max,
              by = 5L))
}

## HAS_TESTS
make_breaks_integer_month_quarter <- function(age, break_max, open_last) {
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE)
        if (!open_last)
            break_max <- break_max + 1L
    }
    seq.int(from = 0L,
            to = break_max)
}

## HAS_TESTS
make_breaks_integer_year <- function(age, width, break_max, open_last) {
    if (is.null(break_max)) {
        break_max <- max(age, na.rm = TRUE)
        if (open_last)
            break_max <- (break_max %/% width) * width
        else
            break_max <- (break_max %/% width + 1L) * width
    }
    seq.int(from = 0L,
            to = break_max,
            by = width)
}

## HAS_TESTS
make_fill <- function(fill, X, INDEX) {
    stopifnot(is.data.frame(INDEX))
    stopifnot(all(sapply(INDEX, is.factor)))
    stopifnot(identical(length(X), nrow(INDEX)))
    if (is.null(fill)) {
        if (!is.null(X)) { 
            if (identical(length(X), 0L))
                return(0L)
            ## try to infer value of 'fill'
            X_obs <- stats::na.omit(X)
            if (length(X_obs) > 0L) {
                is_pos <- X_obs > 0L
                is_int <- X_obs == round(X_obs)
                if (all(is_pos & is_int))
                    return(0L)
                if (any(!is_pos))
                    return(NA_integer_)
            }
        }
        n_possible_combn <- prod(sapply(INDEX, nlevels))
        n_actual_combn <- nrow(unique(INDEX))
        if (n_actual_combn < n_possible_combn)
            stop(gettextf(paste("some combinations of the cross-classifying variables are not",
                                "included in the data, but no value for '%s' has been supplied"),
                          "fill"),
                 call. = FALSE)
        return(0L)
    }
    else {
        demcheck::err_is_length_1(x = fill,
                                  name = "fill")
        if (is.na(fill))
            return(NA_integer_)
        if (is.numeric(fill)) {
            if (fill == round(fill))
                fill <- as.integer(fill)
            return(fill)
        }
        stop(gettextf("invalid value for '%s'",
                      "fill"))
    }
}

## HAS_TESTS
make_labels_age_group_month_quarter <- function(break_min,
                                                break_max,
                                                open_last,
                                                unit,
                                                include_na) {
    l <- demcheck::err_tdy_break_min_max_integer(break_min = break_min,
                                                 break_max = break_max,
                                                 null_ok = FALSE,
                                                 equal_ok = open_last)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    suffix <- switch(unit,
                     month = "m",
                     quarter = "q",
                     stop(gettextf("can't handle unit '%s'",
                                   unit)))
    if (break_max > break_min) {
        s <- seq.int(from = break_min,
                     to = break_max - 1L)
        ans_mid <- sprintf("%d%s", s, suffix)
    }
    else
        ans_mid <- NULL
    if (open_last)
        ans_right <- sprintf("%d%s+", break_max, suffix)
    else
        ans_right <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_mid, ans_right, ans_na)
    ans
}

## HAS_TESTS
make_labels_cohort_month_quarter <- function(break_min,
                                             break_max,
                                             open_first,
                                             unit,
                                             include_na) {
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    l <- demcheck::err_tdy_break_min_max_date(break_min = break_min,
                                              break_max = break_max,
                                              unit = unit,
                                              null_ok = FALSE,
                                              equal_ok = open_first)
    break_min <- l$break_min
    break_max <- l$break_max
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")    
    s <- seq.Date(from = break_min,
                  to = break_max,
                  by = unit)
    year <- format(s, format = "%Y")
    if (unit == "month")
        suffix <- months(s, abbreviate = TRUE)
    else if (unit == "quarter")
        suffix <- quarters(s)
    else
        stop(gettextf("can't handle unit '%s'",
                      unit))
    n <- length(s)
    ans_mid <- paste(year[-n], suffix[-n])
    if (open_first)
        ans_first <- sprintf("<%s %s", year[[1L]], suffix[[1L]])
    else
        ans_first <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_first, ans_mid, ans_na)
    ans
}

## HAS_TESTS
make_labels_period_month_quarter <- function(break_min,
                                             break_max,
                                             unit,
                                             include_na) {
    l <- demcheck::err_tdy_break_min_max_date(break_min = break_min,
                                              break_max = break_max,
                                              unit = unit,
                                              null_ok = FALSE,
                                              equal_ok = FALSE)
    break_min <- l$break_min
    break_max <- l$break_max
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")    
    s <- seq.Date(from = break_min,
                  to = break_max,
                  by = unit)
    year <- format(s, format = "%Y")
    if (unit == "month")
        suffix <- months(s, abbreviate = TRUE)
    else if (unit == "quarter")
        suffix <- quarters(s)
    else
        stop(gettextf("can't handle unit '%s'",
                      unit))
    n <- length(s)
    ans_mid <- paste(year[-n], suffix[-n])
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_mid, ans_na)
    ans
}







    
