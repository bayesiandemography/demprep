
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
date_to_period_or_cohort_custom <- function(date,
                                            breaks,
                                            open_first,
                                            as_factor) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    breaks <- demcheck::err_tdy_breaks_date(x = breaks,
                                            name = "breaks",
                                            open_first = open_first)
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (!open_first)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = breaks[1L])
    labels <- make_labels_period(breaks = breaks,
                                 open_first = open_first,
                                 label_year_start = TRUE)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
    ans
}

## HAS_TESTS
date_to_period_or_cohort_month <- function(date,
                                           break_min,
                                           open_first,
                                           as_factor) {
    demcheck::err_is_positive_length(x = date,
                                     name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    if (!is.null(break_min)) {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
    }
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    breaks <- make_breaks_date_month(date = date,
                                     break_min = break_min)
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_period_month(break_min = break_min,
                                       break_max = break_max,
                                       open_first = open_first)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
    ans   
}

## HAS_TESTS
date_to_period_or_cohort_multi <- function(date,
                                           width,
                                           origin,
                                           month_start,
                                           break_min,
                                           open_first,
                                           as_factor) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    if (is.null(break_min)) {
        origin <- demcheck::err_tdy_integer_scalar(x = origin,
                                                   name = "origin")
        month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                     name = "month_start")
    }
    else {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
        origin <- as.integer(format(break_min, "%Y"))
        month_start <- format(break_min, "%b")
    }
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (!is.null(break_min) && !open_first)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    breaks <- make_breaks_date_year(date = date,
                                    month_start = month_start,
                                    width = width,
                                    origin = origin,
                                    break_min = break_min)
    labels <- make_labels_period(breaks = breaks,
                                      open_first = open_first,
                                      label_year_start = TRUE)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
    ans

}

## HAS_TESTS
date_to_period_or_cohort_quarter <- function(date,
                                             break_min,
                                             open_first,
                                             as_factor) {
    demcheck::err_is_positive_length(x = date,
                                     name = "date")
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    if (!is.null(break_min)) {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
    }
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    breaks <- make_breaks_date_quarter(date = date,
                                       break_min = break_min)
    n <- length(breaks)
    break_min <- breaks[[1L]]
    break_max <- breaks[[n]]
    labels <- make_labels_period_quarter(break_min = break_min,
                                         break_max = break_max,
                                         open_first = open_first)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
    ans   
}

## HAS_TESTS
date_to_period_or_cohort_year <- function(date,
                                          month_start,
                                          label_year_start,
                                          break_min,
                                          open_first,
                                          as_factor) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    if (is.null(break_min)) {
        month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                     name = "month_start")
    }
    else {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
        month_start <- format(break_min, format = "%b")
    }
    demcheck::err_is_logical_flag(x = label_year_start,
                                  name = "label_year_start")
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (open_first && !label_year_start)
        stop(gettextf("'%s' is %s but '%s' is %s",
                      "open_first", "TRUE", "label_year_start", "FALSE"))
    if (!is.null(break_min) && !open_first)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    breaks <- make_breaks_date_year(date = date,
                                    month_start = month_start,
                                    width = 1L,
                                    origin = NULL,
                                    break_min = break_min)
    labels <- make_labels_period(breaks = breaks,
                                 open_first = open_first,
                                 label_year_start = label_year_start)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_first)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels)
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
    ## date_from
    if (is.null(break_min)) {
        date_first <- min(date, na.rm = TRUE)
        date_first_ymd <- as_ymd(date_first)
        year_first <- date_first_ymd$y
        month_first <- date_first_ymd$m
        date_from <- sprintf("%d-%d-01", year_first, month_first)
        date_from <- as.Date(date_from)
    }
    else
        date_from <- break_min
    ## date_to
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
    ## sequence
    seq.Date(from = date_from,
             to = date_to,
             by = "month")
}

## HAS_TESTS
make_breaks_date_quarter <- function(date, break_min) {
    ## date_from
    if (is.null(break_min)) {
        date_first <- min(date, na.rm = TRUE)
        date_first_ymd <- as_ymd(date_first)
        year_first <- date_first_ymd$y
        month_first <- date_first_ymd$m
        year_from <- year_first
        month_from <- ((month_first - 1L) %/% 3L) * 3L + 1L
        date_from <- sprintf("%d-%d-01", year_from, month_from)
        date_from <- as.Date(date_from)
    }
    else
        date_from <- break_min
    ## date_to
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
    ## obtain 'year_origin', 'month_origin', 'day_origin'
    if (is.null(origin)) {
        if (!identical(width, 1L))
            stop(gettextf("'%s' is %s but '%s' equals %d",
                          "origin", "NULL", "width", width))
        origin <- 2000L
    }
    n <- length(date)
    year_origin <- origin
    month_origin <- match(month_start, month.abb)
    day_origin <- 1L
    ## obtain 'year_from'
    if (is.null(break_min)) {
        if (n == 0L) {
            year_from <- year_origin
        }
        else {
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
    }
    else {
        year_from <- format(break_min, "%Y")
        year_from <- as.integer(year_from)
    }
    ## obtain 'year_to'
    if (n == 0L) {
        year_to <- year_from + width
    }
    else {        
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
        else {
            year_to <- year_origin + ((diff_last_origin - 1L) %/% width + 1L) * width
        }
    }
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
                                                 null_ok = FALSE)
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    suffix <- switch(unit,
                     month = "m",
                     quarter = "q",
                     stop(gettextf("can't handle unit '%s'",
                                   unit)))
    s <- seq.int(from = break_min,
                 to = break_max - 1L)
    ans_mid <- sprintf("%d%s", s, suffix)
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
make_labels_period_month_quarter <- function(break_min,
                                             break_max,
                                             open_first,
                                             unit,
                                             include_na) {
    l <- demcheck::err_tdy_break_min_max_date(break_min = break_min,
                                              break_max = break_max,
                                              unit = unit,
                                              null_ok = FALSE)
    break_min <- l$break_min
    break_max <- l$break_max
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
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







    
