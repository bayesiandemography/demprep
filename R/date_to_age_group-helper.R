
## assume inputs all valid
age_to_label <- function(age, min, max, width, breaks,
                         open_left, open_right, as_factor) {
    labels <- make_age_labels(min = min,
                              max = max,
                              width = width,
                              breaks = breaks,
                              open_left = open_left,
                              open_right = open_right)
    if (is.null(breaks))
        breaks <- seq.int(from = min,
                          to = max,
                          by = width)
    i <- findInterval(x = age, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}




## assume inputs all valid
age_to_label_month <- function(age, min, max,
                               open_left, open_right, as_factor) {
    labels <- make_age_labels_month(min = min,
                                    max = max,
                                    open_left = open_left,
                                    open_right = open_right)
    breaks <- seq.int(from = min, to = max)
    i <- findInterval(x = age, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}


## assume inputs all valid
age_to_label_quarter <- function(age, min, max,
                                 open_left, open_right, as_factor) {
    labels <- make_age_labels_quarter(min = min,
                                      max = max,
                                      open_left = open_left,
                                      open_right = open_right)
    breaks <- seq.int(from = min, to = max)
    i <- findInterval(x = age, vec = breaks)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
    if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}


    


## 'date' and 'dob' POSIXlt vectors with equal
## length and no NAs - typically constructed
## using 'err_tdy_date_dob'
date_to_age_completed_months <- function(date, dob) {
    y2 <- date$year + 1900L
    y1 <- dob$year + 1900L
    m2 <- date$mon + 1L
    m1 <- dob$mon + 1L
    d2 <- date$mday
    d1 <- dob$mday
    is_29feb_2 <- (m2 == 2L) & (d2 == 29L)
    is_29feb_1 <- (m1 == 2L) & (d1 == 29L)
    d2[is_29_feb_2] <- 28L
    d1[is_29_feb_1] <- 28L
    12L * (y2 - y1) + (m2 - m1) - (d2 < d1)
}
