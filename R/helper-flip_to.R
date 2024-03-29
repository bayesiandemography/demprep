
## HAS_TESTS
flip_to_internal <- function(x,
                             to_end,
                             month_start) {
    month_start <- demcheck::err_tdy_month_start(x = month_start,
                                                 name = "month_start")
    do_nothing <- (identical(month_start, "Jan")
        || identical(length(x), 0L)
        || all(is.na(x)))
    if (do_nothing) {
        return(x)
    }
    labels_x <- if (is.factor(x)) levels(x) else unique(x)
    parsed <- parse_integers(x = labels_x,
                             name = "x")
    low <- parsed$low
    up <- parsed$up
    is_open_first <- parsed$is_open_first
    is_open_last <- parsed$is_open_last
    i_open_last <- match(TRUE, is_open_last, nomatch = 0L)
    if (i_open_last > 0L)
        stop(gettextf("'%s' has interval [\"%s\"] that is open on the right",
                      "x", labels_x[[i_open_last]]),
             call. = FALSE)
    if (to_end) {
        labels_ans <- up
        labels_ans[is_open_first] <- paste0("<", up[is_open_first] + 1L)
    }
    else {
        labels_ans <- low - 1L
        labels_ans[is_open_first] <- paste0("<", up[is_open_first] - 1L)
    }
    if (is.factor(x))
        ans <- factor(x,
                      levels = labels_x,
                      labels = labels_ans,
                      exclude = NULL)
    else {
        if (is.numeric(x)) {
            if (is.integer(x))
                labels_ans <- as.integer(labels_ans)
            else
                labels_ans <- as.numeric(labels_ans)
        }
        ans <- labels_ans[match(x, labels_x)]
    }
    ans
}
