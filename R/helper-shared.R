
## Helper functions used by many other functions
## in this package


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

age_completed_months <- function(date, dob) {
    date <- as_ymd(date)
    dob <- as_ymd(dob)
    y2 <- date$y
    m2 <- date$m
    d2 <- date$d
    y1 <- dob$y
    m1 <- dob$m
    d1 <- dob$d
    is_29feb_date <- (m2 == 2L) & (d2 == 29L)
    is_29feb_date <- (m1 == 2L) & (d1 == 29L)
    d2[is_29_feb_2] <- 28L
    d1[is_29_feb_1] <- 28L
    12L * (y2 - y1) + (m2 - m1) - (d2 < d1)
}



