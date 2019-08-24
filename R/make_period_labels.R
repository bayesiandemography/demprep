
#' Make period labels
#' 
#' @rdname make_period_labels
NULL

#' @rdname make_period_labels
#' @export
make_period_labels <- function(min, max, open_left, open_right) {
}


#' @rdname make_period_labels
#' @export
make_period_labels_month <- function(min, max, open_left, open_right) {
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


#' @rdname make_period_labels
#' @export
make_period_labels_quarter <- function(min, max, open_left, open_right) {
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

