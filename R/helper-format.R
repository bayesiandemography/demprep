
## NO_TESTS
format_age_month_quarter_year <- function(x,
                                         break_min,
                                         break_max,
                                         open_last) {
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_break_max <- !is.null(break_max)
    ## check arguments
    if (has_break_min)
        break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                                   name = "break_min")
    if (has_break_max)
        break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                               name = "break_max")
    if (has_break_min && has_break_max) {
        demcheck::err_lt_scalar(x1 = break_min,
                                x2 = break_max,
                                name1 = "break_min",
                                name2 = "break_max")
        demcheck::err_is_logical_flag(x = open_last,
                                      name = "open_last")
    }
    ## deal with "empty" case where 'x' has no non-NA values
    ## and 'break_min' or 'break_max' is missing
    ## (so cannot construct levels)
    n <- length(x)
    if (n == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        is_unbounded <- is.null(break_min) || is.null(break_max)
        if (is_unbounded) {
            ans <- rep(NA_character_, times = n)
            ans <- factor(ans,
                          levels = NA_character_,
                          exclude = NULL)
            return(ans)
        }
    }
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## turn labels into integers, and extract information about these integers
    parsed <- parse_integers(x = labels_x,
                             name = "x")
    low <- parsed$low # integer
    up <- parsed$up   # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_min_x <- parsed$break_min # integer
    break_max_x <- parsed$break_max # integer
    i_open_first <- match(TRUE, is_open_first, nomatch = 0L)
    if (i_open_first > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
                      "x", labels_x[[i_open_first]]),
             call. = FALSE)
    }
    ## if 'break_min' is supplied, make sure that all intervals finish
    ## at or above 'break_min'
    if (has_break_min) {
        is_too_low_min <- low < break_min
        i_too_low_min <- match(TRUE, is_too_low_min, nomatch = 0L)
        if (i_too_low_min > 0L) {
            stop(gettextf("'%s' has interval [\"%s\"] that starts below '%s' [%d]",
                          "x", labels_x[[i_too_low_min]], "break_min", break_min),
                 call. = FALSE)
        }
    }
    ## if 'open_last' is TRUE, and 'break_max' is supplied, and there are open intervals,
    ## check that the open intervals all start at or above 'break_max'
    if (open_last && has_break_max && any(is_open_last)) {
        is_too_low_max <- is_open_last & (low < break_max)
        i_too_low_max <- match(TRUE, is_too_low_max, nomatch = 0L)
        if (i_too_low_max > 0L) {
            stop(gettextf("'%s' has open interval [\"%s\"] that starts below '%s' [%d]",
                          "x", labels_x[[i_too_low_max]], "break_max", break_max),
                 call. = FALSE)
        }
    }
    ## if 'open_last' is FALSE, check that there are no open intervals
    if (!open_last) {
        i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
        if (i_open_last > 0L)
            stop(gettextf("'%s' is %s but '%s' has open interval [\"%s\"]",
                          "open_last", "FALSE", "x", labels_x[[i_open_last]]),
                 call. = FALSE)
    }
    ## if 'open_last' is FALSE, and 'break_max' is supplied,
    ## make sure that all intervals less than 'break_max'
    if (!open_last && has_break_max) {
        is_too_high <- up >= break_max
        i_too_high <- match(TRUE, is_too_high, nomatch = 0L)
        if (i_too_high > 0L) {
            stop(gettextf("'%s' has interval [\"%s\"] that ends above '%s' [%d]",
                          "x", labels_x[[i_too_high]], "break_max", break_max),
                 call. = FALSE)
        }
    }
    ## make 'break_min', 'break_max'
    if (!has_break_min) {
        break_min <- break_min_x
        message(gettextf("setting '%s' to %d",
                         "break_min", break_min))
    }
    if (!has_break_max) {
        break_max <- break_max_x
        message(gettextf("setting '%s' to %d",
                         "break_max", break_max))
    }
    ## make 'breaks'
    breaks <- seq.int(from = break_min,
                      to = break_max)
    ## make labels for these breaks
    include_na <- anyNA(labels_old)
    labels_new <- make_labels_age(breaks = breaks,
                                  open_last = open_last,
                                  include_na = include_na)
    ## make return value
    if (open_last) {
        n <- length(labels_new)
        i <- match(x, labels_new, nomatch = n)  # unrecognized labels belong to open interval
        ans <- labels_new[i]
    }
    else
        ans <- x
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}

## NO_TESTS
format_cohort_month_quarter_year <- function(x,
                               break_min,
                               open_first,
                               parse_fun,
                               label_fun) {
    ## see if arguments supplied
    has_break_min <- !is.null(break_min)
    has_open_first <- !is.null(open_first)
    ## check arguments
    if (has_break_min) {
        break_min <- demcheck::err_tdy_non_negative_integer_scalar(x = break_min,
                                                                   name = "break_min")
    }
    if (has_open_first) {
        demcheck::err_is_logical_flag(x = open_first,
                                      name = "open_first")
    }
    ## deal with "empty" cases where 'x'
    ## has length 0 or is all NA
    if (length(x) == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        ans <- factor(x,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## parse the labels
    parsed <- parse_fun(x = labels_x,
                             name = "x")
    low <- parsed$low
    up <- parsed$up
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_min_x <- parsed$break_min
    break_max_x <- parsed$break_max
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", labels_x[[i_open_last]]),
             call. = FALSE)
    }
    ## do checks required when 'open_first' was supplied
    if (has_open_first) {
        ## if 'open_first' is TRUE, and 'break_min' is supplied, and there are open intervals,
        ## check that the open intervals all start at or below 'break_min'
        if (open_first && has_break_min && any(is_open_first)) {
            is_too_high <- is_open_first & (up > break_min)
            i_too_high <- match(TRUE, is_too_high, nomatch = 0L)
            if (i_too_high > 0L) {
                if (inherits(break_min, "Date"))
                    break_min_str <- sprintf("\"%s\"", break_min)
                else
                    break_min_str <- sprintf("%d", break_min)
                stop(gettextf("'%s' has open interval [\"%s\"] that ends above '%s' [%s]",
                              "x", labels_x[[i_too_high]], "break_min", break_min_str),
                     call. = FALSE)
            }
        }
        ## if 'open_first' is FALSE, check that there are no open intervals
        if (!open_first) {
            i_is_open <- match(TRUE, is_open_first, nomatch = 0L)
            if (i_is_open > 0L)
                stop(gettextf("'%s' is %s but '%s' has open interval [\"%s\"]",
                              "open_first", "FALSE", "x", labels_x[[i_is_open]]),
                     call. = FALSE)
        }
    }
    else { ## where 'open_first' not supplied, assign a default value
        open_first <- any(is_open_first) || has_break_min
        message(gettextf("setting '%s' to %s",
                         "open_first", open_first))
    }
    ## make break_min, break_max
    if (!has_break_min) {
        break_min <- break_min_x
        message(gettextf("setting '%s' to %d",
                         "break_min", break_min))
    }
    break_max <- break_max_x
    ## make labels
    include_na <- anyNA(labels_x)
    labels_new <- labels_fun(break_min = break_min,
                             break_max = break_max,
                             open_first = open_first,
                             include_na = include_na)
    ## make return value
    if (open_first) {
        i <- match(x, labels_new, nomatch = 1L)  # unrecognized label belong to open interval
        ans <- labels_new[i]
    }
    else
        ans <- x
    ans <- factor(x = ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}


## NO_TESTS
format_period_month_quarter_year <- function(x,
                                             parse_fun,
                                             labels_fun) {
    ## deal with "empty" cases where 'x'
    ## has length 0 or is all NA
    if (length(x) == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        ans <- factor(x,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## parse the labels
    parsed <- parse_fun(x = labels_x,
                        name = "x")
    low <- parsed$low
    up <- parsed$up
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_min <- parsed$break_min
    break_max <- parsed$break_max
    i_open <- match(TRUE, is_open_first | is_open_last, nomatch = 0L)
    if (i_open > 0L) {
        stop(gettextf("'%s' has open interval [\"%s\"]",
                      "x", labels_x[[i_open]]),
             call. = FALSE)
    }
    ## make labels
    include_na <- anyNA(labels_x)
    labels_new <- labels_fun(break_min = break_min,
                             break_max = break_max,
                             include_na = include_na)
    ans <- factor(x = x,
                  levels = labels_new,
                  exclude = NULL)
    ans
}



extract_time_quarter <- function(labels) {
    ## regular expressions
    p_single <- "^-?[0-9]+ Q[1-4]$"
    p_open <- "^<-?[0-9]+ Q[1-4]$"
    ## classify labels, rasing error for invalid ones
    is_na <- is.na(labels)
    is_single <- grepl(p_single, labels)
    is_open <- grepl(p_open, labels)
    is_valid <- is_na | is_single | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" is not a valid label",
                      labels[[i_invalid]]),
             call. = FALSE)
    ## extract lower and upper times
    time_low <- rep(NA_integer_, times = length(labels))
    time_up <- time_low
    time_single <- as.integer(labels[is_single])
    if (disambiguate_single && !label_year_start && (month_start != "Jan"))
        time_single <- time_single - 1L
    time_low[is_single] <- time_single
    time_up[is_single] <- time_single + 1L
    time_up[is_open] <- as.integer(sub("<", "", labels[is_open]))
    if (low_up) {
        time_low[is_low_up] <- as.integer(sub(p_low_up, "\\1", labels[is_low_up]))
        time_up[is_low_up] <- as.integer(sub(p_low_up, "\\2", labels[is_low_up]))
        demcheck::err_interval_diff_ge_one(int_low = time_low,
                                           int_up = time_up,
                                           is_low_up = is_low_up,
                                           labels = labels)
    }
    list(time_low = time_low,
         time_up = time_up,
         is_open = is_open)
}




format_triangle_month_quarter_year <- function(x,
                                               age,
                                               break_max,
                                               open_last) {
    valid_triangles <- c("Lower", "Upper", NA)
    ## see if arguments supplied
    has_break_max <- !is.null(break_max)
    ## check arguments
    demcheck::err_length_same(x1 = age,
                              x2 = x,
                              name1 = "age",
                              name2 = "x")
    if (has_break_max) {
        break_max <- demcheck::err_tdy_positive_integer_scalar(x = break_max,
                                                               name = "break_max")
    }
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    ## deal with "empty" case where 'x' has no non-NA values
    n <- length(x)
    if (n == 0L) {
        ans <- factor(character(),
                      levels = c("Lower", "Upper"))
        return(ans)
    }
    is_na_any <- is.na(x) | is.na(age)
    if (all(is_na_any)) {
        ans <- rep(NA_character_, times = n)
        ans <- factor(ans,
                      levels = c("Lower", "Upper", NA),
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'labels' vectors
    labels_x <- unique(x)
    labels_age <- unique(age)
    ## check for invalid triangles
    is_valid_tri <- labels_x %in% valid_triangles
    i_invalid_tri <- match(FALSE, is_valid_tri, nomatch = 0L)
    if (i_invalid_tri > 0L)
        stop(gettextf("'%s' has invalid value for Lexis triangle [\"%s\"]",
                      "x", labels_x[[i_invalid_tri]]),
             call. = FALSE)
    ## parse 'age'
    parsed <- parse_integers(x = labels_age,
                             name = "age")
    low <- parsed$low # integer
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    break_max_age <- parsed$break_max # integer
    i_open_first <- match(TRUE, is_open_first, nomatch = 0L)
    if (i_open_first > 0L) {
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the left",
                      "age", labels_age[[i_open_first]]),
             call. = FALSE)
    }
    ## if 'open_last' is TRUE and 'break_max' is supplied, check that
    ## all open age groups start at or above 'break_max'
    if (open_last && has_break_max) {
        is_too_low <- is_open_last & (low < break_max)
        i_too_low <- match(TRUE, is_too_low, nomatch = 0L)
        if (i_too_low > 0L) {
            stop(gettextf("'%s' has open interval [\"%s\"] that starts below '%s' [%d]",
                          "age", labels_age[[i_too_low]], "break_min", break_min),
                 call. = FALSE)
        }
    }
    ## if 'open_last' is FALSE, check that there are no open intervals
    if (!open_last) {
        i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
        if (i_open_last > 0L)
            stop(gettextf("'%s' is %s but '%s' has open interval [\"%s\"]",
                          "open_last", "FALSE", "age", labels_age[[i_open_last]]),
                 call. = FALSE)
    }
    ## if 'open_last' is FALSE, and 'break_max' is supplied,
    ## make sure that all intervals less than 'break_max'
    if (!open_last && has_break_max) {
        is_too_high <- up >= break_max
        i_too_high <- match(TRUE, is_too_high, nomatch = 0L)
        if (i_too_high > 0L) {
            stop(gettextf("'%s' has interval [\"%s\"] that ends above '%s' [%d]",
                          "age", labels_age[[i_too_high]], "break_max", break_max),
                 call. = FALSE)
        }
    }
    ## make 'break_max'
    if (!has_break_max) {
        break_max <- break_max_age
        message(gettextf("setting '%s' to %d",
                         "break_max", break_max))
    }
    ## Make the return value.
    ## Note that if 'break_max' was determined by the data,
    ## then there are no triangles in the open age group,
    ## so no need to reclassify any of them
    ans <- x
    if (has_break_max) {
        low_all <- low[match(age, labels_age)]
        is_open_not_first <- !is.na(age) & (low_all >= break_max + 1L)
        ans[is_open_not_first] <- "Upper"
    }
    ## return result
    levels <- c("Lower", "Upper")
    if (anyNA(ans))
        levels <- c(levels, NA)
    ans <- factor(x = ans,
                  levels = levels,
                  exclude = NULL)
    ans
}

    
    
