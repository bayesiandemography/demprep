
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



parse_integers_intervals <- function(x,
                                     name,
                                     month_start,
                                     label_year_start) {
    ## whether to subtract 1 from single-year labels
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
    low[is_low_up] <- as.integer(sub(p_low_up, "\\1", x[is_low_up]))
    up[is_low_up] <- as.integer(sub(p_low_up, "\\2", x[is_low_up]))
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
    low[is_low_up] <- as.integer(sub(p_low_up, "\\1", x[is_low_up]))
    up[is_low_up] <- as.integer(sub(p_low_up, "\\2", x[is_low_up]))
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





