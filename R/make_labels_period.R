
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
    demcheck::err_is_first_day_unit(x = breaks,
                                    name = "breaks",
                                    unit = "year")
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = year_to,
                                  name = "year_to")
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

make_period_labels_month_quarter <- function(min_break,
                                             max_break,
                                             open_left,
                                             open_right,
                                             unit,
                                             include_na) {
    min_break <- demcheck::err_tdy_date_scalar(x = min_break,
                                                               name = "min_break")
    max_break <- demcheck::err_tdy_date_scalar(x = max_break,
                                                           name = "max_break")
    demcheck::err_is_gt_scalar(x1 = max_break, ## extend to allow dates
                               x2 = min_break,
                               name1 = "max_break",
                               name2 = "min_break")
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    demcheck::err_is_logical_flag(x = include_na,
                                  name = "include_na")
    format <- switch(unit,
                     month = "%Y %b",
                     quarter = "%Y Q%q",
                     stop(gettextf("can't handle unit '%s'",
                                   unit)))
    s <- seq.Date(from = min_break,
                  to = max_break - 1L,
                  by = "unit")
    
    ans_mid <- sprintf("%d%s", s, suffix)
    if (open_left)
        ans_left <- paste0("<", ans_mid[[1]])
    else
        ans_left <- NULL
    if (open_right)
        ans_right <- sprintf("%d%s+", max_break, suffix)
    else
        ans_right <- NULL
    if (include_na)
        ans_na <- NA_character_
    else
        ans_na <- NULL
    ans <- c(ans_left, ans_mid, ans_right, ans_na)
    ans
}





#' @rdname make_labels_period
#' @export
make_labels_period_quarter <- function(min_break,
                                       max_break,
                                       open_left = FALSE,
                                       open_right = FALSE,
                                       include_na = FALSE) {
    make_period_labels_month_quarter(min_break = min_break,
                                     max_break = max_break,
                                     open_left = open_left,
                                     open_right = open_right,
                                     unit = "quarter",
                                     include_na)
}




#' @rdname make_labels_period
#' @export
make_labels_period_month <- function(min_break,
                                     max_break,
                                     open_left = FALSE,
                                     open_right = FALSE,
                                     include_na = FALSE) {
    make_period_labels_month_quarter(min_break = min_break,
                                     max_break = max_break,
                                     open_left = open_left,
                                     open_right = open_right,
                                     unit = "month",
                                     include_na)
}


