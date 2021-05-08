
## HAS_TESTS
parse_integers <- function(x, name) {
    ## classify labels, rasing error for invalid ones
    is_na <- is.na(x)
    is_single <- grepl(CONST_P_SINGLE, x)
    is_open_first <- grepl(CONST_P_OPEN_FIRST, x)
    is_open_last <- grepl(CONST_P_OPEN_LAST, x)
    is_valid <- is_na | is_single | is_open_first | is_open_last
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("'%s' has invalid label [\"%s\"]",
                      name, x[[i_invalid]]),
             call. = FALSE)
    ## extract 'low' and 'up'
    n <- length(x)
    low <- rep(NA_integer_, times = n)
    up <- rep(NA_integer_, times = n)
    low[is_single] <- as.integer(x[is_single])
    up[is_single] <- low[is_single] + 1L
    up[is_open_first] <- as.integer(sub("<", "", x[is_open_first]))
    low[is_open_last] <- as.integer(sub("\\+", "", x[is_open_last]))
    ## find 'break_min' and 'break_max'
    if (all(is_na)) {
        break_min <- NA_integer_
        break_max <- NA_integer_
    }
    else {
        if (any(is_open_first))
            break_min <- max(up[is_open_first])
        else
            break_min <- min(low, na.rm = TRUE)
        if (any(is_open_last))
            break_max <- min(low[is_open_last])
        else
            break_max <- max(up, na.rm = TRUE)
    }
    ## return answer
    list(low = low,
         up = up,
         is_open_first = is_open_first,
         is_open_last = is_open_last,
         break_min = break_min,
         break_max = break_max)
}

## HAS_TESTS
parse_integers_intervals <- function(x,
                                     name,
                                     month_start,
                                     label_year_start) {
    ## whether to subtract 1 from single-year labels
    ## to get implied calendar years
    subtract_1 <- (month_start != "Jan") && !label_year_start
    ## classify labels, rasing error for invalid ones
    is_na <- is.na(x)
    is_single <- grepl(CONST_P_SINGLE, x)
    is_low_up <- grepl(CONST_P_LOW_UP, x)
    is_open_first <- grepl(CONST_P_OPEN_FIRST, x)
    is_open_last <- grepl(CONST_P_OPEN_LAST, x)
    is_valid <- is_na | is_single | is_low_up | is_open_first | is_open_last
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("'%s' has invalid label [\"%s\"]",
                      name, x[[i_invalid]]),
             call. = FALSE)
    ## extract 'low' and 'up'
    n <- length(x)
    low <- rep(NA_integer_, times = n)
    up <- rep(NA_integer_, times = n)
    low[is_single] <- as.integer(x[is_single])
    up[is_single] <- low[is_single] + 1L
    if (subtract_1) {
        low[is_single] <- low[is_single] - 1L
        up[is_single] <- up[is_single] - 1L
    }
    low[is_low_up] <- as.integer(sub(CONST_P_LOW_UP, "\\1", x[is_low_up]))
    up[is_low_up] <- as.integer(sub(CONST_P_LOW_UP, "\\2", x[is_low_up]))
    is_up_le_low <- up[is_low_up] <= low[is_low_up]
    i_up_le_low <- match(TRUE, is_up_le_low, nomatch = 0L)
    if (i_up_le_low > 0L)
        stop(gettextf("'%s' has label with upper limit less than or equal to lower limit [\"%s\"]",
                      name, x[is_low_up][[i_up_le_low]]),
             call. = FALSE)
    up[is_open_first] <- as.integer(sub("<", "", x[is_open_first]))
    low[is_open_last] <- as.integer(sub("\\+", "", x[is_open_last]))
    ## find 'break_min' and 'break_max'
    if (all(is_na)) {
        break_min <- NA_integer_
        break_max <- NA_integer_
    }
    else {
        if (any(is_open_first))
            break_min <- max(up[is_open_first])
        else
            break_min <- min(low, na.rm = TRUE)
        if (any(is_open_last))
            break_max <- min(low[is_open_last])
        else
            break_max <- max(up, na.rm = TRUE)
    }
    ## return answer
    list(low = low,
         up = up,
         is_open_first = is_open_first,
         is_open_last = is_open_last,
         break_min = break_min,
         break_max = break_max)
}


## HAS_TESTS
parse_quantities <- function(x, name) {
    ## classify labels, rasing error for invalid ones
    is_na <- is.na(x)
    is_single <- grepl(CONST_P_SINGLE, x)
    is_low_up <- grepl(CONST_P_LOW_UP, x)
    is_open_first <- grepl(CONST_P_OPEN_FIRST, x)
    is_open_last <- grepl(CONST_P_OPEN_LAST, x)
    is_valid <- is_na | is_single | is_low_up | is_open_first | is_open_last
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("'%s' has invalid label [\"%s\"]",
                      name, x[[i_invalid]]),
             call. = FALSE)
    ## extract 'low' and 'up'
    n <- length(x)
    low <- rep(NA_integer_, times = n)
    up <- rep(NA_integer_, times = n)
    low[is_single] <- as.integer(x[is_single])
    up[is_single] <- low[is_single] + 1L
    low[is_low_up] <- as.integer(sub(CONST_P_LOW_UP, "\\1", x[is_low_up]))
    up[is_low_up] <- as.integer(sub(CONST_P_LOW_UP, "\\2", x[is_low_up])) + 1L
    is_up_le_low <- up[is_low_up] <= low[is_low_up]
    i_up_le_low <- match(TRUE, is_up_le_low, nomatch = 0L)
    if (i_up_le_low > 0L)
        stop(gettextf("'%s' has label with upper limit less than lower limit [\"%s\"]",
                      name, x[is_low_up][[i_up_le_low]]),
             call. = FALSE) ## "upper limit" in error message is not 'up'
    up[is_open_first] <- as.integer(sub("<", "", x[is_open_first]))
    low[is_open_last] <- as.integer(sub("\\+", "", x[is_open_last]))
    ## find 'break_min' and 'break_max'
    if (all(is_na)) {
        break_min <- NA_integer_
        break_max <- NA_integer_
    }
    else {
        if (any(is_open_first))
            break_min <- max(up[is_open_first])
        else
            break_min <- min(low, na.rm = TRUE)
        if (any(is_open_last))
            break_max <- min(low[is_open_last])
        else
            break_max <- max(up, na.rm = TRUE)
    }
    ## return answer
    list(low = low,
         up = up,
         is_open_first = is_open_first,
         is_open_last = is_open_last,
         break_min = break_min,
         break_max = break_max)
}


## HAS_TESTS
parse_quarters <- function(x, name) {
    ## classify labels, rasing error for invalid ones
    is_na <- is.na(x)
    is_single <- grepl(CONST_P_SINGLE_QUARTER, x)
    is_open_first <- grepl(CONST_P_OPEN_FIRST_QUARTER, x)
    is_open_last <- grepl(CONST_P_OPEN_LAST_QUARTER, x)
    is_valid <- is_na | is_single | is_open_first | is_open_last
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("'%s' has invalid label [\"%s\"]",
                      name, x[[i_invalid]]),
             call. = FALSE)
    ## extract 'low' and 'up'
    n <- length(x)
    low <- as.Date(rep(NA, times = n))
    up <- as.Date(rep(NA, times = n))
    low[is_single] <- date_start_quarter(x[is_single])
    up[is_single] <- rollforward_quarter(low[is_single])
    up[is_open_first] <- date_start_quarter(x[is_open_first])
    low[is_open_last] <- date_start_quarter(x[is_open_last])
    ## find 'break_min' and 'break_max'
    if (all(is_na)) {
        break_min <- as.Date(NA)
        break_max <- as.Date(NA)
    }
    else {
        if (any(is_open_first))
            break_min <- max(up[is_open_first])
        else
            break_min <- min(low, na.rm = TRUE)
        if (any(is_open_last))
            break_max <- min(low[is_open_last])
        else
            break_max <- max(up, na.rm = TRUE)
    }
    ## return answer
    list(low = low,
         up = up,
         is_open_first = is_open_first,
         is_open_last = is_open_last,
         break_min = break_min,
         break_max = break_max)
}


## HAS_TESTS
parse_months <- function(x, name) {
    ## classify labels, rasing error for invalid ones
    is_na <- is.na(x)
    is_single <- grepl(CONST_P_SINGLE_MONTH, x)
    is_open_first <- grepl(CONST_P_OPEN_FIRST_MONTH, x)
    is_open_last <- grepl(CONST_P_OPEN_LAST_MONTH, x)
    is_valid <- is_na | is_single | is_open_first | is_open_last
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("'%s' has invalid label [\"%s\"]",
                      name, x[[i_invalid]]),
             call. = FALSE)
    ## extract 'low' and 'up'
    n <- length(x)
    low <- as.Date(rep(NA, times = n))
    up <- as.Date(rep(NA, times = n))
    low[is_single] <- date_start_month(x[is_single])
    up[is_single] <- rollforward_month(low[is_single])
    up[is_open_first] <- date_start_month(x[is_open_first])
    low[is_open_last] <- date_start_month(x[is_open_last])
    ## find 'break_min' and 'break_max'
    if (all(is_na)) {
        break_min <- as.Date(NA)
        break_max <- as.Date(NA)
    }
    else {
        if (any(is_open_first))
            break_min <- max(up[is_open_first])
        else
            break_min <- min(low, na.rm = TRUE)
        if (any(is_open_last))
            break_max <- min(low[is_open_last])
        else
            break_max <- max(up, na.rm = TRUE)
    }
    ## return answer
    list(low = low,
         up = up,
         is_open_first = is_open_first,
         is_open_last = is_open_last,
         break_min = break_min,
         break_max = break_max)
}



