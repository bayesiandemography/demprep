
infer_dimscale_period_month <- function(labels) {
    ## regexp patterns
    p_open_first <- "^<[0-9]{4} [A-z]{3}$"
    p_mid <- "^[0-9]{4} [A-z]{3}$"
    p_open_last <- "^[0-9]{4} [A-z]{3}\\+$"
    ## must have at least one label
    demcheck::err_is_positive_length(x = labels,
                                     name = "labels")
    ## only need to check one instance of each label
    labels <- unique(labels)
    ## initial, non-definitive check that labels have expected format
    is_open_first <- grepl(p_open_first, labels)
    is_mid <- grepl(p_mid, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_na <- is.na(labels)
    is_invalid <- !(is_open_first | is_mid | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" is not a valid label for a period of one month",
                        labels[[i_invalid]]))
    open_first <- any(is_open_first)
    open_last <- any(is_open_last)
    include_na <- any(is_na)
    ## turn labels into dates, raising error if this cannot be done
    labels_trimmed <- labels
    labels_trimmed[is_open_first] <- sub("^<", "", labels_trimmed[is_open_first])
    labels_trimmed[is_open_last] <- sub("\\+$", "", labels_trimmed[is_open_last])
    dates <- suppressWarnings(as.Date(paste(labels_trimmed, "01"), "%Y %b %d"))
    is_invalid <- is.na(dates) & !is_na
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" is not a valid label for a period of one month",
                        labels[[i_invalid]]))
    ## identify 'break_min' and 'break_max' and make sure
    ## that any open intervals are consistent with them
    i_min <- which.min(dates)
    i_max <- which.max(dates)
    break_min <- dates[[i_min]]
    break_max <- dates[[i_max]]
    if (!open_last)
        break_max <- seq.Date(from = break_max,
                              by = "month",
                              length.out = 2L)[[2L]]
    is_invalid <- dates[is_open_first] != break_min
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("label \"%s\" is open on the left, but label \"%s\" refers to an earlier date",
                      labels[is_open_first][[i_invalid]],
                      labels[[i_min]]))
    is_invalid <- dates[is_open_last] != break_max
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("label \"%s\" is open on the right, but label \"%s\" refers to a later date",
                      labels[is_open_last][[i_invalid]],
                      labels[[i_max]]))
    labels_expected <- make_labels_period_month_quarter(break_min = break_min,
                                                        break_max = break_max,
                                                        open_first = open_first,
                                                        open_last = open_last,
                                                        unit = "month",
                                                        include_na = include_na)
    is_not_included <- !(labels_expected %in% labels)
    i_not_included <- match(TRUE, is_not_included, nomatch = 0L)
    if (i_not_included > 0L)
        return(gettextf("period labels have a gap : no label for period \"%s\"",
                      labels_expected[[i_not_included]]))
    list(break_min = break_min,
         break_max = break_max,
         open_first = open_first,
         open_last = open_last,
         include_na = include_na)
}
