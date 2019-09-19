
#' Make age labels
#'
#' @name make_labels_age_group
NULL

## HAS_TESTS
#' @rdname make_labels_age_group
#' @export
make_labels_age_group_year <- function(breaks,
                                       open_left = FALSE,
                                       open_right = TRUE,
                                       include_na = FALSE) {
    breaks <- demcheck::err_tdy_breaks_integer(x = breaks,
                                               name = "breaks")
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    n <- length(breaks)
    if (n == 0L) {
        ans_mid <- character()
        if (open_left)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_left", "TRUE"))
        if (open_right)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_right", "TRUE"))
    }
    else {
        ans_mid <- character(length = n - 1L)
        diff <- diff(breaks)
        is_single_unit <- diff == 1L
        ans_mid[is_single_unit] <- breaks[-n][is_single_unit]
        if (any(!is_single_unit)) {
            lower <- breaks[-n][!is_single_unit]
            upper <- breaks[-1L][!is_single_unit] - 1L
            ans_mid[!is_single_unit] <- paste(lower, upper, sep = "-")
        }
    }
    if (open_left)
        ans_left <- paste0("<", breaks[[1L]])
    else
        ans_left <- NULL
    if (open_right)
        ans_right <- paste0(breaks[[n]], "+")
    else
        ans_right <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    c(ans_left, ans_mid, ans_right, ans_na)
}

## HAS_TESTS
#' @rdname make_labels_age_group
#' @export
make_labels_age_group_quarter <- function(min_break = 0,
                                          max_break = 400,
                                          open_left = FALSE,
                                          open_right = TRUE,
                                          include_na = FALSE) {
    make_labels_age_group_month_quarter(min_break = min_break,
                                        max_break = max_break,
                                        open_left = open_left,
                                        open_right = open_right,
                                        unit = "quarter",
                                        include_na = include_na)
}

## HAS_TESTS
#' @rdname make_labels_age_group
#' @export
make_labels_age_group_month <- function(min_break = 0,
                                        max_break = 1200,
                                        open_left = FALSE,
                                        open_right = TRUE,
                                        include_na = FALSE) {
    make_labels_age_group_month_quarter(min_break = min_break,
                                        max_break = max_break,
                                        open_left = open_left,
                                        open_right = open_right,
                                        unit = "month",
                                        include_na = include_na)
}


