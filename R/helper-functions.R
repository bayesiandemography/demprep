

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

is_lower_within_month <- function(date_ymd, dob_ymd) {
    ((date_ymd$d - 1L) %/% 2L) >= (dob_ymd$d %/% 2L)
}

age_completed_months_start_month <- function(date_ymd, dob_ymd) {
    (12L * (date_ymd$y - dob_ymd$y)
        + (date_ymd$m - dob_ymd$m)
        - (dob_ymd$d != 1L))
}


    
    




