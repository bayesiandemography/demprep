
#' Make period labels
#' 
#' @name make_labels_period
NULL

## HAS_TESTS
#' @rdname make_labels_period
#' @export
make_labels_period_year <- function(breaks,
                                    open_left = FALSE,
                                    open_right = FALSE,
                                    year_to = TRUE,
                                    include_na = FALSE) {
    breaks <- demcheck::err_tdy_breaks_date(x = breaks,
                                            name = "breaks")
    demcheck::err_is_first_day_unit_vector(x = breaks,
                                           name = "breaks",
                                           unit = "year")
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
        breaks_annual <- seq.Date(from = breaks[[1L]],
                                  by = "year",
                                  length.out = n)
        is_annual <- isTRUE(all.equal(breaks, breaks_annual))
        is_1_jan <- identical(format(breaks[[1L]], "%m-%d"), "01-01")
        head <- breaks[-n]
        tail <- breaks[-1L]
        if (is_annual) {
            demcheck::err_is_logical_flag(x = year_to,
                                          name = "year_to")
            if (year_to && !is_1_jan)
                ans_mid <- format(tail, "%Y")
            else
                ans_mid <- format(head, "%Y")
        }
        else {
            ans_mid <- paste(format(head, "%Y"),
                             format(tail, "%Y"),
                             sep = "-")
        }
    }
    if (open_left) {
        if (is_annual)
            ans_left <- paste0("<", ans_mid[[1L]])
        else
            ans_left <- paste0("<", format(head[[1L]], "%Y"))
    }
    else
        ans_left <- NULL
    if (open_right) {
        if (is_annual)
            ans_right <- paste0(ans_mid[[n - 1L]], "+")
        else
            ans_right <- paste0(format(tail[[n - 1L]], "%Y"), "+")
    }
    else
        ans_right <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    c(ans_left, ans_mid, ans_right, ans_na)
}





#' @rdname make_labels_period
#' @export
make_labels_period_quarter <- function(break_min,
                                       break_max,
                                       open_left = FALSE,
                                       open_right = FALSE,
                                       include_na = FALSE) {
    make_labels_period_month_quarter(break_min = break_min,
                                     break_max = break_max,
                                     open_left = open_left,
                                     open_right = open_right,
                                     unit = "quarter",
                                     include_na)
}




#' @rdname make_labels_period
#' @export
make_labels_period_month <- function(break_min,
                                     break_max,
                                     open_left = FALSE,
                                     open_right = FALSE,
                                     include_na = FALSE) {
    make_labels_period_month_quarter(break_min = break_min,
                                     break_max = break_max,
                                     open_left = open_left,
                                     open_right = open_right,
                                     unit = "month",
                                     include_na)
}
