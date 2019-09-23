
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
date_to_period_or_cohort_multi <- function(date,
                                           width,
                                           origin,
                                           first_month,
                                           break_min,
                                           open_left,
                                           as_factor) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    width <- demcheck::err_tdy_positive_integer_scalar(x = width,
                                                       name = "width")
    origin <- demcheck::err_tidy_integer_scalar(x = origin,
                                                name = "origin")
    if (!is.null(break_min)) {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
        demcheck::err_date_consistent_with_width_origin(x = break_min,
                                                        name = "break_min",
                                                        width = width,
                                                        origin = origin)
        demcheck::err_is_logical_flag(x = open_left,
                                      name = "open_left")
    }
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (!is.null(break_min) && !open_left)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    if (!open_left)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    breaks <- make_breaks_date_year(date = date,
                                    first_month = first_month,
                                    width = width,
                                    origin = origin,
                                    break_min = break_min)
    include_na <- any(is.na(date))
    labels <- make_labels_period_year(breaks = breaks,
                                      open_left = open_left,
                                      open_right = FALSE,
                                      year_to = NULL,
                                      include_na = include_na)
    date_int <- as.integer(date)

    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
    ans   

}



chk_date_consistent_with_first_month <- function(x, name, first_month) {
    x_month <- format(x, format = "%b")
    if (!identical(x_month, first_month)) {
        first_month_full <- month.name[match(first_month, month.abb)]
        x_month_full <- format(x, format = "%B")
        return(gettextf(paste("'%s' [\"%s\"] implies that year starts in %s,",
                              "but '%s' implies that year starts in %s"),
                        name,
                        x,
                        x_month_full,
                        "first_month",
                        first_month_full))
    }
    TRUE
}


chk_date_consistent_with_width_origin <- function(x, name, origin, width) {
    x_year <- format(x, format = "%Y")
    x_year <- as.integer(x_year)
    if ((origin - x_year) %% width != 0L)
        return(gettextf("'%s' [\"%s\"] inconsistent with '%s' [%d] and '%s' [%d]",
                        name,
                        x,
                        "origin",
                        origin,
                        "width",
                        width))
    TRUE
}

        

## HAS_TESTS
date_to_period_or_cohort_year <- function(date,
                                          first_month,
                                          year_to,
                                          break_min,
                                          open_left,
                                          as_factor) {
    date <- demcheck::err_tdy_date_vector(x = date,
                                          name = "date")
    first_month <- demcheck::err_tdy_first_month(x = first_month,
                                                 name = "first_month")
    demcheck::err_is_logical_flag(x = year_to,
                                  name = "year_to")
    if (!is.null(break_min)) {
        demcheck::err_is_length_1(x = break_min,
                                  name = "break_min")
        break_min <- demcheck::err_tdy_date_scalar(x = break_min,
                                                   name = "break_min")
        demcheck::err_date_consistent_with_first_month(x = break_min,
                                                       name = "break_min",
                                                       first_month = first_month)
        demcheck::err_is_logical_flag(x = open_left,
                                      name = "open_left")
    }
    demcheck::err_is_logical_flag(x = as_factor,
                                  name = "as_factor")
    if (!is.null(break_min) && !open_left)
        demcheck::err_ge_break_min_date(date = date,
                                        break_min = break_min)
    breaks <- make_breaks_date_year(date = date,
                                    first_month = first_month,
                                    width = 1L,
                                    origin = NULL,
                                    break_min = break_min)
    include_na <- any(is.na(date))
    labels <- make_labels_period_year(breaks = breaks,
                                      open_left = open_left,
                                      open_right = FALSE,
                                      year_to = year_to,
                                      include_na = include_na)
    date_int <- as.integer(date)
    breaks_int <- as.integer(breaks)
    i <- findInterval(x = date_int,
                      vec = breaks_int)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(x = ans,
                      levels = labels,
                      exclude = NULL)
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
make_breaks_date_year <- function(date,
                                  first_month,
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
    year_origin <- origin
    month_origin <- match(first_month, month.abb)
    day_origin <- 1L
    ## obtain 'year_from'
    if (is.null(break_min)) {
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
    else {
        year_from <- format(break_min, "%Y")
        year_from <- as.integer(year_from)
    }
    ## obtain 'year_to'
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
make_breaks_integer_lifetab <- function(break_max) {
    c(0L,
      1L,
      seq.int(from = 5L,
              to = break_max,
              by = 5L))
}

## HAS_TESTS
make_breaks_integer_year <- function(age, width, break_max, open_right) {
    if (!is.null(break_max))
        break_max <- break_max
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

## HAS_TESTS
make_labels_age_group_month_quarter <- function(break_min,
                                                break_max,
                                                open_left,
                                                open_right,
                                                unit,
                                                include_na) {
    l <- demcheck::err_tdy_break_min_max_integer(break_min = break_min,
                                                 break_max = break_max,
                                                 null_ok = FALSE)
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
    s <- seq.int(from = break_min,
                 to = break_max - 1L)
    ans_mid <- sprintf("%d%s", s, suffix)
    if (open_left)
        ans_left <- paste0("<", ans_mid[[1]])
    else
        ans_left <- NULL
    if (open_right)
        ans_right <- sprintf("%d%s+", break_max, suffix)
    else
        ans_right <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_left, ans_mid, ans_right, ans_na)
    ans
}

## HAS_TESTS
make_labels_period_month_quarter <- function(break_min,
                                             break_max,
                                             open_left,
                                             open_right,
                                             unit,
                                             include_na) {
    l <- demcheck::err_tdy_break_min_max_date(break_min = break_min,
                                              break_max = break_max,
                                              unit = unit,
                                              null_ok = FALSE)
    break_min <- l$break_min
    break_max <- l$break_max
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
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
    if (open_left)
        ans_left <- sprintf("<%s %s", year[[1L]], suffix[[1L]])
    else
        ans_left <- NULL
    if (open_right)
        ans_right <- sprintf("%s %s+", year[[n]], suffix[[n]])
    else
        ans_right <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_left, ans_mid, ans_right, ans_na)
    ans
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


    
    




