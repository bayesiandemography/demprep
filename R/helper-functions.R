

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
make_breaks_integer_year <- function(age, age_max, open_right) {
    if (is.finite(age_max))
        break_max <- age_max
    else {
        break_max <- max(age,
                         na.rm = TRUE)
        if (!open_right)
            break_max <- break_max + 1L
    }
    seq.int(from = 0L,
            to = break_max)
}

## HAS_TESTS
make_breaks_integer_multi <- function(age, width, age_max, open_right) {
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


    
    




