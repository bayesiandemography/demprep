
#' Make age labels
#'
#' @name make_labels_age_group
NULL

## HAS_TESTS
#' Make labels for age groups measured in years
#'
#' 
#' @rdname make_labels_age_group
#' @export
make_labels_age_group <- function(breaks,
                                  open_first = FALSE,
                                  open_last = TRUE,
                                  include_na = FALSE) {
    breaks <- demcheck::err_tdy_breaks_integer(x = breaks,
                                               name = "breaks",
                                               open_first = open_first,
                                               open_last = open_last)
    demcheck::err_is_logical_flag(x = open_first,
                                  name = "open_first")
    demcheck::err_is_logical_flag(x = open_last,
                                  name = "open_last")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    n <- length(breaks)
    if (n == 0L) {
        ## 'err_tdy_breaks_integer' checked that 'open_first' and 'open_last' both FALSE
        ans_mid <- character()
        ans_left <- NULL
        ans_right <- NULL
    }
    else if (n == 1L) {
        ## 'err_tdy_breaks_integer' checked that 'open_first' or 'open_last' TRUE
        ans_mid <- as.character(breaks)
        if (open_first)
            ans_left <- paste0("<", ans_mid)
        else
            ans_left <- NULL
        if (open_last)
            ans_right <- paste0(ans_mid, "+")
        else
            ans_right <- NULL
    }
    else {
        ## 'open_first' or 'open_last' may be TRUE
        ans_mid <- character(length = n - 1L)
        diff <- diff(breaks)
        is_single_year <- diff == 1L
        ans_mid[is_single_year] <- breaks[-n][is_single_year]
        if (any(!is_single_year)) {
            lower <- breaks[-n][!is_single_year]
            upper <- breaks[-1L][!is_single_year] - 1L
            ans_mid[!is_single_year] <- paste(lower, upper, sep = "-")
        }
    }
    if (open_first)
        ans_left <- paste0("<", breaks[[1L]])
    else
        ans_left <- NULL
    if (open_last)
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
make_labels_age_group_quarter <- function(break_min = 0,
                                          break_max = 400,
                                          open_first = FALSE,
                                          open_last = TRUE,
                                          include_na = FALSE) {
    make_labels_age_group_month_quarter(break_min = break_min,
                                        break_max = break_max,
                                        open_first = open_first,
                                        open_last = open_last,
                                        unit = "quarter",
                                        include_na = include_na)
}

## HAS_TESTS
#' @rdname make_labels_age_group
#' @export
make_labels_age_group_month <- function(break_min = 0,
                                        break_max = 1200,
                                        open_first = FALSE,
                                        open_last = TRUE,
                                        include_na = FALSE) {
    make_labels_age_group_month_quarter(break_min = break_min,
                                        break_max = break_max,
                                        open_first = open_first,
                                        open_last = open_last,
                                        unit = "month",
                                        include_na = include_na)
}


