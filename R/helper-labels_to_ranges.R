

to_date_ranges_month_quarter_year <- function(x,
                                              parse_fun) {
    ## deal with "empty" cases where 'x'
    ## has length 0 or is all NA
    if (length(x) == 0L) {
        ans <- factor()
        return(ans)
    }
    if (all(is.na(x))) {
        ans <- factor(x,
                      levels = NA_character_,
                      exclude = NULL)
        return(ans)
    }
    ## put unique values in 'labels_x' vector
    labels_x <- unique(x)
    ## parse the labels
    parsed <- parse_fun(x = labels_x,
                        name = "x")
    low <- parsed$low
    up <- parsed$up
    ## make labels
    low_up <- mapply(c, low, up, SIMPLIFY = FALSE)
    labels_new <- make_labels_dateranges(low_up)
    i_label <- match(x, labels_x)
    ans <- labels_new[i_label]
    ans <- factor(ans,
                  levels = labels_new,
                  exclude = NULL)
    ans
}
