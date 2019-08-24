

## assume inputs all valid
date_to_label <- function(date, min, max, width, breaks,
                          open_left, open_right, as_factor) {
}



## assume inputs all valid
date_to_label_month <- function(date, min, max,
                                open_left, open_right,
                                as_factor) {
    labels <- make_period_labels_month(min = min,
                                       max = max,
                                       open_left = open_left,
                                       open_right = open_right)
    breaks <- seq.Date(from = min, to = max)
    x <- as.integer(date)
    vec <- as.integer(breaks)
    i <- findInterval(x = x, vec = vec)
    if (open_left)
        i <- i + 1L
    ans <- labels[i]
        if (as_factor)
        ans <- factor(ans, levels = labels)
    ans
}


## assume inputs all valid
date_to_label_quarter <- function(date, min, max,
                                  open_left, open_right,
                                  as_factor) {
}

    
 
