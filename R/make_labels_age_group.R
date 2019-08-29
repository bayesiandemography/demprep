
#' Make age labels
#'
#' @rdname make_labels_age_group
NULL

#' @rdname make_labels_age_group
#' @export
make_labels_age_group <- function(breaks,
                                  open_left = FALSE,
                                  open_right = TRUE) {
    breaks <- err_tdy_breaks_integer(breaks)
    err_is_logical_flag(x = open_left,
                        name = "open_left")
    err_is_logical_flag(x = open_right,
                        name = "open_right")
    n <- length(breaks)
    if (n == 0L) {
        if (open_left)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          breaks, 0L, "open_left", "TRUE"))
        if (open_right)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          breaks, 0L, "open_right", "TRUE"))
        return(character())
    }
    ans_mid <- character(length = n - 1L)
    diff <- diff(breaks)
    is_single_unit <- diff == 1L
    ans_mid[is_single_unit] <- breaks[-n][is_single_unit]
    if (any(!is_single_unit)) {
        lower <- breaks[-n][!is_single_unit]
        upper <- breaks[-1L][!is_single_unit] - 1L
        ans_mid[!is_single_unit] <- paste(lower, upper, sep = "-")
    }
    if (is_open_left)
        ans_left <- paste0("<", breaks[[1L]])
    else
        ans_left <- NULL
    if (is_open_right)
        ans_right <- paste0(breaks[[n]], "+")
    else
        ans_right <- NULL
    c(ans_left, ans_mid, ans_right)
}

#' @rdname make_labels_age_group
#' @export
make_labels_age_group_month <- function(min_break = 0,
                                        max_break = 1200,
                                        open_left = FALSE,
                                        open_right = TRUE) {
    make_labels_age_group_month_quarter(min_break = min_break,
                                        max = max,
                                        open_left = open_left,
                                        open_right = open_right,
                                        unit = "month")
}


#' @rdname make_labels_age_group
#' @export
make_labels_age_group_quarter <- function(min_break = 0,
                                          max_break = 400,
                                          open_left = FALSE,
                                          open_right = TRUE) {
    make_labels_age_group_month_quarter(min_break = min_break,
                                        max_break = max_break,
                                        open_left = open_left,
                                        open_right = open_right,
                                        unit = "quarter")
}

make_age_labels_month_quarter <- function(min_break,
                                          max_break,
                                          open_left,
                                          open_right,
                                          unit = c("month", "quarter")) {
    l <- demcheck::err_tdy_min_max_break(min_break = min_break,
                                         max_break = max_break)
    min_break <- l$min_break
    max_break <- l$max_break
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
    s <- seq.int(from = min_break,
                 to = max_break - 1L)
    ans_mid <- sprintf("%d%s", s, suffix)
    if (is_open_left)
        ans_left <- paste0("<", ans_mid[[1]])
    else
        ans_left <- NULL
    if (is_open_right)
        ans_right <- sprintf("%d%s+", max_break, suffix)
    else
        ans_right <- NULL
    ans <- c(ans_left, ans_mid, ans_right)
    ans
}

