
## NO_TESTS
as_date_range_custom_multi <- function(x,
                                       month_start) {
    ## check arguments
    if (!is.vector(x))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    ## deal with "empty" cases where 'x'
    ## has length 0 or is all NA
    if (length(x) == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        ans <- factor(x,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'levels_x' vector
    if (is.factor(x))
        levels_x <- levels(x)
    else
        levels_x <- unique(x)
    ## parse the labels
    parsed <- parse_intervals(x = x,
                              name = "x")
    year_low <- parsed$low
    year_up <- parsed$up
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", levels_x[[i_open_last]]),
             call. = FALSE)
    }
    ## create dates
    date_low <- ifelse(is.na(year_low),
                       NA,
                       paste(year_low, month_start, 1))
    date_up <- ifelse(is.na(year_up),
                      NA,
                      paste(year_up, month_start, 1))
    date_low <- as.Date(date_low, format = "%Y %b %d")
    date_up <- as.Date(date_up, format = "%Y %b %d")
    ## make new labels
    x_new <- mapply(c, date_low, date_up, SIMPLIFY = FALSE)
    levels_x_new <- make_labels_dateranges(x_new)
    ## put in order
    i <- order_low_up(low = year_low,
                      up = year_up)
    levels_x_ordered <- levels_x[i]
    levels_x_new_ordered <- levels_x_new[i]
    ## make return value
    ans <- factor(x,
                  levels = levels_x_ordered,
                  labels = levels_x_new_ordered,
                  exclude = NULL)
    ans
}


## NO_TESTS
as_date_range_month_quarter <- function(x, parse_fun) {
    ## check arguments
    if (!is.vector(x))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    ## deal with "empty" cases where 'x'
    ## has length 0 or is all NA
    if (length(x) == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        ans <- factor(x,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'levels_x' vector
    if (is.factor(x))
        levels_x <- levels(x)
    else
        levels_x <- unique(x)
    ## parse the labels
    parsed <- parse_fun(x = x,
                        name = "x")
    date_low <- parsed$low
    date_up <- parsed$up
    is_open_last <- parsed$is_open_last
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", levels_x[[i_open_last]]),
             call. = FALSE)
    }
    ## make new labels
    x_new <- mapply(c, date_low, date_up, SIMPLIFY = FALSE)
    levels_x_new <- make_labels_dateranges(x_new)
    ## put in order
    i <- order_low_up(low = date_low,
                      up = date_up)
    levels_x_ordered <- levels_x[i]
    levels_x_new_ordered <- levels_x_new[i]
    ## make return values
    ans <- factor(x,
                  levels = levels_x_ordered,
                  labels = levels_x_new_ordered,
                  exclude = NULL)
    ans
}


## HAS_TESTS
order_low_up <- function(low, up) {
    is_na_low <- is.na(low)
    is_na_up <- is.na(up)
    is_open_left <- is_na_low & !is_na_up
    is_low_up <- !is_na_low & !is_na_up
    is_open_right <- !is_na_low & is_na_up
    val <- ifelse(is_open_left, up, low)
    ans <- order(!is_open_left, !is_low_up, !is_open_right, val)
    ans
}
    
