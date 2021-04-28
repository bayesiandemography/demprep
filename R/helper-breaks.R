

## HAS_TESTS
## Infer 'break_min' from pairs of dates,
## typically derived from quarter labels.
## If all dates are NA. All return values
## have class 'Date'
## Do calculations on integers because 'sapply'
## coerces "Date" objects to numeric.
make_break_min_pairs_date <- function(pairs) {
    origin <- as.Date("1970-01-01")
    pairs_int <- lapply(pairs, function(x) as.integer(x - origin))
    date_low_int <- vapply(X = pairs_int, FUN = `[[`, 1L, FUN.VALUE = 0L)
    date_up_int <- vapply(X = pairs_int, FUN = `[[`, 2L, FUN.VALUE = 0L)
    is_na_low <- sapply(date_low_int, is.na)
    is_na_up <- sapply(date_up_int, is.na)
    is_na <- is_na_low & is_na_up
    is_open_first <- is_na_low & !is_na_up
    is_non_na <- !is_na & !is_open_first
    if (any(is_open_first))
        ans <- max(date_up_int[is_open_first])
    else if (any(is_non_na))
        ans <- min(date_low_int[is_non_na])
    else
        ans <- NA
    ans <- as.Date(ans, origin = origin)
    ans
}


## HAS_TESTS
## Infer 'break_max' from pairs of pairs,
## typically derived from quarter labels.
## If all pairs are NA. All return values
## have class 'Date'
## Do calculations on integers because 'sapply'
## coerces "Date" objects to numeric.
make_break_max_pairs_date <- function(pairs) {
    origin <- as.Date("1970-01-01")
    pairs_int <- lapply(pairs, function(x) as.integer(x - origin))
    date_low_int <- vapply(X = pairs_int, FUN = `[[`, 1L, FUN.VALUE = 0L)
    date_up_int <- vapply(X = pairs_int, FUN = `[[`, 2L, FUN.VALUE = 0L)
    is_na_low <- is.na(date_low_int)
    is_na_up <- is.na(date_up_int)
    is_na <- is_na_low & is_na_up
    is_open_last <- !is_na_low & is_na_up
    is_non_na <- !is_na & !is_open_last
    if (any(is_open_last))
        ans <- min(date_low_int[is_open_last])
    else if (any(is_non_na))
        ans <- max(date_up_int[is_non_na])
    else
        ans <- NA
    ans <- as.Date(ans, origin = origin)
    ans
}



## date to date ---------------------------------------------------------------

## HAS_TESTS
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
    month_to <- month_max + 1L
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

## HAS_TESTS
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
    if (month_to > 12L) {
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

## HAS_TESTS
make_breaks_date_to_date_year <- function(date,
                                          month_start) {
    month_start_int <- match(month_start, month.abb)
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


## date to integer ------------------------------------------------------------

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


## label to date ------------------------------------------------------------

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


## label to integer ------------------------------------------------------------

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

