
#' Internal functions for making labels
#'
#' Functions to create labels for cross-classifying
#' variables. These functions would not normally
#' be called directly by end users.
#'
#' The argument \code{breaks}, or the arguments
#' \code{break_min} and \code{break_max},
#' are used to specify intervals via their boundaries.
#' The arguments \code{int_min} and \code{int_max}
#' are used to specify integer-based intervals directly.
#'
#' @param labels A character vector.
#' @param breaks A vector of integers or dates.
#' @param break_min An integer or date.
#' @param break_max An integer or date.
#' @param int_min An integer.
#' @param int_max An integer.
#' @param unit Time unit. Currently \code{"quarter"}
#' or \code{"month"}.
#' @param open_first Logical. Whether to
#' include interval open on left.
#' @param open_last Logical. Whether to
#' include interval open on right.
#' @param include_na Logical. Whether to
#' append \code{NA} to end of labels.
#'
#' @return A character vector.
#'
#' @keywords internal
#'
#' @name make_labels-internal
NULL


## HAS_TESTS
#' @rdname make_labels-internal
#' @export
make_labels_default <- function(labels, include_na) {
    ans <- as.character(labels)
    if (include_na)
        ans <- c(ans, NA_character_)
    ans
}

## HAS_TESTS
#' @rdname make_labels-internal
#' @export
make_labels_integers <- function(int_min, int_max, include_na) {
    s <- seq.int(from = int_min,
                 to = int_max)
    ans_mid <- as.character(s)
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_mid, ans_na)
    ans
}

## HAS_TESTS
#' @rdname make_labels-internal
#' @export
make_labels_grouped_int_enumerations <- function(breaks, open_first, open_last, include_na) {
    n <- length(breaks)
    if (n == 0L) {
        ans_mid <- character()
        ans_first <- NULL
        ans_last <- NULL
    }
    else if (n == 1L) {
        ans_mid <- NULL
        if (open_first)
            ans_first <- paste0("<", breaks)
        else
            ans_first <- NULL
        if (open_last)
            ans_last <- paste0(breaks, "+")
        else
            ans_last <- NULL
    }
    else {
        ans_mid <- character(length = n - 1L)
        diff <- diff(breaks)
        is_single <- diff == 1L
        ans_mid[is_single] <- breaks[-n][is_single]
        if (any(!is_single)) {
            lower <- breaks[-n][!is_single]
            upper <- breaks[-1L][!is_single] - 1L
            ans_mid[!is_single] <- paste(lower, upper, sep = "-")
        }
        if (open_first)
            ans_first <- paste0("<", breaks[[1L]])
        else
            ans_first <- NULL
        if (open_last)
            ans_last <- paste0(breaks[[n]], "+")
        else
            ans_last <- NULL
    }
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_first, ans_mid, ans_last, ans_na)
    ans
}

## HAS_TESTS
#' @rdname make_labels-internal
#' @export
make_labels_grouped_int_endpoints <- function(breaks, open_first, include_na) {
    n <- length(breaks)
    if (n == 0L) {
        ans_mid <- character()
        ans_first <- NULL
        ans_last <- NULL
    }
    else if (n == 1L) {
        ans_mid <- NULL
        if (open_first)
            ans_first <- paste0("<", breaks)
        else
            ans_first <- NULL
    }
    else {
        ans_mid <- character(length = n - 1L)
        lower <- breaks[-n]
        upper <- breaks[-1L]
        ans_mid <- paste(lower, upper, sep = "-")
        if (open_first)
            ans_first <- paste0("<", breaks[[1L]])
        else
            ans_first <- NULL
    }
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    c(ans_first, ans_mid, ans_na)
}

## HAS_TESTS
#' @rdname make_labels-internal
#' @export
make_labels_calendar_quarters_months <- function(break_min,
                                                 break_max,
                                                 open_first,
                                                 include_na,
                                                 unit) {
    s <- seq.Date(from = break_min,
                  to = break_max,
                  by = unit)
    year <- format(s, format = "%Y")
    if (unit == "month")
        suffix <- months(s, abbreviate = TRUE)
    else if (unit == "quarter")
        suffix <- quarters(s)
    else
        stop(gettextf("can't handle unit '%s'",
                      unit))
    n <- length(s)
    if (n == 1L)
        ans_mid <- NULL
    else
        ans_mid <- paste(year[-n], suffix[-n])
    if (open_first)
        ans_first <- sprintf("<%s %s", year[[1L]], suffix[[1L]])
    else
        ans_first <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_first, ans_mid, ans_na)
    ans
}

## HAS_TESTS
#' @rdname make_labels-internal
#' @export
make_labels_duration_quarters_months <- function(break_min,
                                                 break_max,
                                                 open_last,
                                                 include_na,
                                                 unit) {
    suffix <- switch(unit,
                     month = "m",
                     quarter = "q",
                     stop(gettextf("can't handle unit '%s'",
                                   unit)))
    if (break_max > break_min) {
        s <- seq.int(from = break_min,
                     to = break_max - 1L)
        ans_mid <- sprintf("%d%s", s, suffix)
    }
    else
        ans_mid <- NULL
    if (open_last)
        ans_last <- sprintf("%d%s+", break_max, suffix)
    else
        ans_last <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_mid, ans_last, ans_na)
    ans
}

