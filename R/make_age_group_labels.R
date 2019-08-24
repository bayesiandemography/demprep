
#' Make age labels
#'
#' @rdname make_age_group_labels
NULL

#' @rdname make_age_group_labels
#' @export
make_age_group_labels <- function(min = 0, max = 100, width = 1,
                            breaks = NULL,
                            open_left = FALSE, open_right = TRUE) {
    use_breaks <- !is.null(breaks)
    if (use_breaks)
        breaks <- demcheck::err_tdy_breaks(breaks)
    else {
        l <- demcheck::err_tdy_min_max(min = min,
                                       max = max)
        min <- l$min
        max <- l$max
        width <- demcheck::err_tdy_width(width = width,
                                         min = min,
                                         max = max)
        breaks <- seq(from = min,
                      to = max,
                      by = width)
    }
    demcheck::err_is_logical_flag(x = open_left,
                                  name = "open_left")
    demcheck::err_is_logical_flag(x = open_right,
                                  name = "open_right")
    n <- length(breaks)
    if (n == 0L)
        return(character())
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

#' @rdname make_age_group_labels
#' @export
make_age_group_labels_month <- function(min = 0, max = 1200,
                                  open_left = FALSE,
                                  open_right = TRUE) {
    make_age_group_labels_month_quarter(min = min,
                                  max = max,
                                  open_left = open_left,
                                  open_right = open_right,
                                  unit = "month")
}


#' @rdname make_age_group_labels
#' @export
make_age_group_labels_quarter <- function(min = 0, max = 400,
                                  open_left = FALSE,
                                  open_right = TRUE) {
    make_age_group_labels_month_quarter(min = min,
                                  max = max,
                                  open_left = open_left,
                                  open_right = open_right,
                                  unit = "quarter")
}
