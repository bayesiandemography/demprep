
#' Make period labels
#' 
#' @rdname make_labels_period
NULL

#' @rdname make_labels_period
#' @export
make_labels_period <- function(breaks,
                               open_left = FALSE,
                               open_right = FALSE,
                               is_year_to = TRUE) {
    breaks <- err_tdy_breaks_date(breaks)
    err_is_logical_flag(x = open_left
                        name = "open_left")
    err_is_logical_flag(x = open_right
                        name = "open_right")
    err_is_logical_flag(x = year_to
                        name = "year__to")
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
    breaks_annual <- seq.Date(from = breaks[[1L]],
                              by = "year",
                              length.out = n)
    is_annual <- isTRUE(all.equal(breaks, breaks_annual))
    head <- breaks[-n]
    tail <- breaks[-1L]
    if (is_annual) {
        if (is_year_to)
            ans_mid <- format(tail, "%Y")
        else
            ans_mid <- format(head, "%Y")
    }
    else {
        ans_mid <- paste(format(head, "%Y"),
                         format(tail, "%Y"),
                         sep = "-")
    }
    if (open_left) {
        if (is_annual)
            ans_left <- sprintf("<%s", ans_mid[[1L]])
        else
            ans_left <- sprintf("<%s", format(head[[1L]], "%Y"))
    }
    else
        ans_left <- NULL
    if (open_right)
        ans_left <- sprintf("<%s", format(breaks[[1L]], "%Y"))
    else
        ans_left <- NULL
}


#' @rdname make_labels_period
#' @export
make_labels_period_month <- function(min_break,
                                     max_break,
                                     open_left = FALSE,
                                     open_right = FALSE) {
    
    fmt <- "%Y %b"
    ans <- format.Date(date, format = fmt)
    if (open_left) {
        label_left <- paste0("<", format(min, format = fmt))
        ans[date < min] <- label_left
    }
    if (open_right) {
        label_right <- paste0(format(min, format = fmt), "+")
        ans[date >= max] <- label_right
    }
    if (as_factor) {
        if (open_left)
            from <- min
        else {
            min <- min(date, na.rm = TRUE)
            from <- as.Date(paste0(format(min, "%Y-%m"), "-01"))
        }
        if (open_right)
            to <- max
        else {
            max <- max(date, na.rm = TRUE)
            to <- as.Date(paste0(format(max, "%Y-m"), "-01"))
        }
        s <- seq.Date(from = from
                      to = to,
                      by = "month")
        labels <- format(s, format = fmt)
        if (open_left)
            labels <- c(label_left, labels)
        if (open_right)
            labels[length(labels)] <- label_right
        ans <- factor(ans, levels = labels)
    }
    ans
}


#' @rdname make_labels_period
#' @export
make_labels_period_quarter <- function(min, max, open_left, open_right) {
    fmt <- "%Y %b"
    ans <- format.Date(date, format = fmt)
    if (open_left) {
        label_left <- paste0("<", format(min, format = fmt))
        ans[date < min] <- label_left
    }
    if (open_right) {
        label_right <- paste0(format(min, format = fmt), "+")
        ans[date >= max] <- label_right
    }
    if (as_factor) {
        if (open_left)
            from <- min
        else {
            min <- min(date, na.rm = TRUE)
            from <- as.Date(paste0(format(min, "%Y-%m"), "-01"))
        }
        if (open_right)
            to <- max
        else {
            max <- max(date, na.rm = TRUE)
            to <- as.Date(paste0(format(max, "%Y-m"), "-01"))
        }
        s <- seq.Date(from = from
                      to = to,
                      by = "month")
        labels <- format(s, format = fmt)
        if (open_left)
            labels <- c(label_left, labels)
        if (open_right)
            labels[length(labels)] <- label_right
        ans <- factor(ans, levels = labels)
    }
    ans
}

