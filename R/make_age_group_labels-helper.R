
make_age_labels_month_quarter <- function(min, max, open_left, open_right,
                                          unit = c("month", "quarter")) {
    l <- demcheck::err_tdy_min_max(x1 = min,
                                   x2 = max,
                                   name1 = min,
                                   name2 = max)
    x1 <- l[[name1]]
    x2 <- l[[name2]]
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    unit <- match.arg(unit)
    suffix <- switch(unit,
                     month = "m",
                     quarter = "q",
                     stop(gettextf("can't handle unit '%s'",
                                   unit)))
    s <- seq.int(from = min, to = max - 1L)
    ans_mid <- sprintf("%d%s", s, suffix)
    if (is_open_left)
        ans_left <- paste0("<", ans_mid[[1]])
    else
        ans_left <- NULL
    if (is_open_right)
        ans_right <- sprintf("%d%s+", max, suffix)
    else
        ans_right <- NULL
    ans <- c(ans_left, ans_mid, ans_right)
    ans
}
