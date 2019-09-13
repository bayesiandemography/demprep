

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

## HAS_TESTS
make_breaks_integer_lifetab <- function(age_max) {
    c(0L,
      1L,
      seq.int(from = 5L,
              to = age_max,
              by = 5L))
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










## possible ------------------------------------------------------------


is_lower_within_month <- function(date_ymd, dob_ymd) {
    ((date_ymd$d - 1L) %/% 2L) >= (dob_ymd$d %/% 2L)
}

age_completed_months_start_month <- function(date_ymd, dob_ymd) {
    (12L * (date_ymd$y - dob_ymd$y)
        + (date_ymd$m - dob_ymd$m)
        - (dob_ymd$d != 1L))
}


    
    




