
## HAS_TESTS
## Add 'n' months to 'date'. Where necessary, 'add_month'
## rolls back to the end of the previous month, to preserve the
## relationship 'month_new - month_old = n'.
## 'date' should have class "Date" and 'n'
## should have class "integer"
add_months <- function(date, n) {
    date_ymd <- as_ymd(date)
    n_month_total <- 12L * (date_ymd$y - 1L) + (date_ymd$m - 1L) + n ## 0-based
    year_ans <- (n_month_total %/% 12L) + 1L ## 1-based
    month_ans <- (n_month_total %% 12L) + 1L
    date_start_month <- sprintf("%d-%d-01",
                                year_ans,
                                month_ans)
    date_start_month <- as.Date(date_start_month)
    n_day_month <- n_day_month(date_start_month)
    day_ans <- pmin(date_ymd$d, n_day_month)
    ans <- sprintf("%d-%d-%d",
                   year_ans,
                   month_ans,
                   day_ans)
    as.Date(ans)
}

## HAS_TESTS
## Add 'n' quarters to 'date'. Where necessary, 'add_quarter'
## rolls back to the end of the previous month, to preserve the
## relationship 'month_new - month_old = 3 * n'.
## 'date' should have class "Date" and 'n'
## should have class "integer"
add_quarters <- function(date, n) {
    n <- 3L * n
    add_months(date = date,
               n = n)
}

## HAS_TESTS
## Add 'n' years to 'date'. Where the old date is 29 February,
## and the new date is not a leap year, 'add_years' rolls back to
## 28 February.
## 'date' should have class "Date" and 'n'
## should have class "integer"
add_years <- function(date, n) {
    date_ymd <- as_ymd(date)
    year_date <- date_ymd$y
    month <- date_ymd$m
    day_date <- date_ymd$d
    date_is_29_feb <- (month == 2) & (day_date == 29L)
    year_ans <- year_date + n
    year_ans_is_leap_year <- is_leap_year(year_ans)
    day_ans <- day_date
    roll_back_29_feb <- date_is_29_feb & !year_ans_is_leap_year
    day_ans[roll_back_29_feb] <- 28L
    ans <- sprintf("%d-%d-%d", year_ans, month, day_ans)
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
## Return coordinates, measured in days,
## for lines representing lifelines. Each
## month on the age axis is 31 days, regardless
## of the length of a month on the time axis.
## To achieve agreement, lifelines are shifted
## upwards by 0-3 days at the end of each month.
## 'date1' and 'dob1' both have length 1
coord_lifeline <- function(date1, dob1) {
    first_boundary <- rollforward_month(dob1)
    boundaries <- seq.Date(from = first_boundary,
                           to = date1,
                           by = "month")
    x <- c(dob1, rep(boundaries, each = 2L), date1)
    diff_y <- as.integer(diff(x))
    first_start <- rollback_month(dob1)
    starts <- seq.Date(from = first_start,
                       by = "month",
                       length.out = length(boundaries))
    n_day_month <- n_day_month(starts)
    shifts <- 31L - n_day_month
    i_shifts <- seq.int(from = 2L,
                        by = 2L,
                        length.out = length(shifts))
    diff_y[i_shifts] <- shifts
    y <- c(0L, cumsum(diff_y))
    n <- length(x)
    list(x0 = x[-n],
         y0 = y[-n],
         x1 = x[-1L],
         y1 = y[-1L])
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
make_breaks_integer_births <- function(age, width, break_min, break_max) {
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
        demcheck::err_length_1(x = fill,
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
## Number of days in month containing 'date'. Handles leap years.
n_day_month <- function(date) {
    n_day <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 31L, 31L, 30L, 31L)
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

## NO_TESTS
plot_date_to_age_triangle <- function(date, dob, unit, breaks_time = NULL, breaks_age,
                                      open_last, labels_time = NULL, labels_age,
                                      show_months, show_vert, show_diag, cex = 0.8) {
    old_par <- graphics::par(mar = c(6, 2, 2, 0),
                             mgp = c(0, 0, 0),
                             cex = cex)
    n_date <- length(date)
    days_per_unit <- switch(unit,
                            month = 31L,
                            quarter = 31L * 3L,
                            year = 31L * 12L,
                            stop("invalid unit"))
    n_br_time <- length(breaks_time)
    n_br_age <- length(breaks_age)
    diff_br_age <- diff(breaks_age)
    date_min <- min(dob, na.rm = TRUE)
    date_min <- rollback_month(date_min)
    if (!is.null(breaks_time))
        date_min <- min(date_min, breaks_time)
    date_max <- max(date, na.rm = TRUE)
    date_max <- rollforward_month(date_max)
    if (!is.null(breaks_time))
        date_max <- max(date_max, breaks_time)
    width_date <- date_max - date_min
    age_approx <- as.integer(date - dob) / days_per_unit
    age_max <- max(breaks_age, 1.05 * age_approx)
    if (open_last) {
        width <- breaks_age[[n_br_age]] - breaks_age[[n_br_age - 1L]]
        age_max  <- max(age_max, breaks_age[[n_br_age]] + width)
    }
    x_plot <- c(date_min - 0.15 * width_date, date_max)
    y_plot <- c(0, age_max)
    ## empty plotting frame
    plot(x = x_plot,
         y = y_plot,
         pch = NA,
         axes = FALSE,
         ylab = "",
         xlab = "")
    ## vertical lines to show boundaries between months (optional)
    if (show_months) {
        boundaries_months <- seq(from = date_min,
                                 to = date_max,
                                 by = "month")
        graphics::segments(x0 = boundaries_months,
                           y0 = 0,
                           x1 = boundaries_months,
                           y1 = breaks_age[[n_br_age]],
                           col = "grey")
        graphics::mtext(text = dob,
                        side = 1,
                        line = -0.5,
                        at = boundaries_months,
                        cex = 0.6,
                        las = 3,
                        col = "grey")
    }
    ## vertical lines and labels to show time breaks (optional)
    if (show_vert) {
        ## lines
        graphics::segments(x0 = breaks_time,
                           y0 = 0,
                           x1 = breaks_time,
                           y1 = age_max,
                           lwd = 0.5,
                           col = "cornflowerblue")
        ## labels for boundaries
        graphics::mtext(text = breaks_time,
                        side = 1,
                        line = -0.5,
                        at = breaks_time,
                        cex = 0.5,
                        las = 3,
                        col = "cornflowerblue")
        ## labels for periods
        labels_time <- sprintf('"%s"', labels_time)
        graphics::mtext(text = labels_time,
                        side = 1,
                        line = 3,
                        at = breaks_time[-n_br_time] + 0.5 * diff(breaks_time),
                        cex = 0.7,
                        las = 1,
                        col = "black")
    }
    ## diagonal lines to show Lexis triangles (optional)
    if (show_diag) {
        ## diagonal lines starting at horizontal axis
        n_age <- n_br_age + open_last
        date_diag_start <- breaks_time[-n_br_time]
        i_date_diag_end <- seq.int(from = n_age,
                                   along.with = date_diag_start)
        i_date_diag_end <- pmin(i_date_diag_end, n_br_time)
        date_diag_end <- breaks_time[i_date_diag_end]
        break_age_min <- min(breaks_age)
        for (i in seq_along(date_diag_start)) {
            date1 <- date_diag_end[[i]]
            dob1 <- date_diag_start[[i]]
            if (date1 > dob1) {
                coord <- coord_lifeline(date1 = date1,
                                        dob1 = dob1)
                x0 <- coord$x0
                y0 <- coord$y0 / days_per_unit
                x1 <- coord$x1
                y1 <- coord$y1 / days_per_unit
                graphics::segments(x0 = x0,
                                   y0 = y0 + break_age_min, # for births
                                   x1 = x1,
                                   y1 = y1 + break_age_min, # for births
                                   lwd = 0.5,
                                   col = "cornflowerblue")
            }
        }
        ## diagonal lines starting at left
        if (open_last) {
            date_diag_start <- rep(breaks_time[[1L]], times = n_br_age - 1L)
            i_end <- pmin(n_br_age : 2L, n_br_time)
            date_diag_end <- breaks_time[i_end]
        }
        else {
            date_diag_start <- rep(breaks_time[[1L]], times = n_br_age - 2L)
            i_end <- pmin((n_br_age - 1L) : 2L, n_br_time)
            date_diag_end <- breaks_time[i_end]
        }
        for (i in seq_along(date_diag_start)) {
            date1 <- date_diag_end[[i]]
            dob1 <- date_diag_start[[i]]
            coord <- coord_lifeline(date1 = date1,
                                    dob1 = dob1)
            x0 <- coord$x0
            y0 <- coord$y0 / days_per_unit + breaks_age[[i + 1L]]
            x1 <- coord$x1
            y1 <- coord$y1 / days_per_unit + breaks_age[[i + 1L]]
            graphics::segments(x0 = x0,
                               y0 = y0,
                               x1 = x1,
                               y1 = y1,
                               lwd = 0.5,
                               col = "cornflowerblue")
        }
    }
    ## horizontal lines to show boundaries between age groups
    y_horiz <- unique(c(0, breaks_age)) # needed for date_to_age_births
    graphics::segments(x0 = rep(date_min, times = n_br_age),
                       y0 = y_horiz,
                       x1 = rep(date_max, times = n_br_age),
                       y1 = y_horiz,
                       lwd = 0.5,
                       lty = "solid",
                       col = "cornflowerblue")
    ## labels for boundaries between age groups
    graphics::text(x = date_min - 0.02 * width_date,
                   y = breaks_age,
                   labels = breaks_age,
                   cex = 0.7,
                   col = "cornflowerblue")
    ## labels for age groups
    labels_age <- sprintf('"%s"', labels_age)
    y_labels_age <- breaks_age[-n_br_age] + 0.5 * diff_br_age
    if (open_last) {
        y_open <- breaks_age[[n_br_age]] + 0.5 * diff_br_age[[n_br_age - 1L]]
        y_labels_age <- c(y_labels_age, y_open)
    }
    graphics::text(x = date_min - 0.04 * width_date,
                   y = y_labels_age,
                   labels = labels_age,
                   pos = 2,
                   cex = 0.9,
                   col = "black")
    ## points for 'dob'
    graphics::points(x = dob,
                     y = rep(0, times = n_date),
                     pch = 19)    
    ## labels for 'dob'
    graphics::mtext(text = dob,
                    side = 1,
                    line = -0.5,
                    at = dob,
                    cex = 0.6,
                    las = 3,
                    col = "black")
    ## life lines, and points for date
    for (i in seq_along(date)) {
        coord <- coord_lifeline(date1 = date[[i]],
                                dob1 = dob[[i]])
        x0 <- coord$x0
        y0 <- coord$y0 / days_per_unit
        x1 <- coord$x1
        y1 <- coord$y1 / days_per_unit
        graphics::segments(x0 = x0,
                           y0 = y0,
                           x1 = x1,
                           y1 = y1)
        graphics::points(x = date[[i]],
                         y = y1[[length(y1)]],
                         pch = 19)
    }        
    ## labels for 'date'
    graphics::mtext(text = date,
                    side = 1,
                    line = -0.5,
                    at = date,
                    cex = 0.6,
                    las = 3,
                    col = "black")
    ## xlab
    graphics::mtext(text = "Time",
                    side = 1,
                    line = 4.5,
                    cex = 0.7,
                    col = "grey35")
    graphics::par(old_par)
    ## ylab
    graphics::mtext(text = "Age",
                    side = 2,
                    line = 2,
                    las = 1,
                    cex = 0.7,
                    col = "grey35")
    graphics::par(old_par)
    invisible(NULL)
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
    breaks <- make_breaks_date_year(date = date,
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
    
