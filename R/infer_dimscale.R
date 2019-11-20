## ## SPLIT INTO SINGLE AND MULTI
## infer_dimscale_period <- function(labels, gaps_ok = FALSE) {
##     ## regexp patterns
##     p_open_first <- "^<[0-9]{4}$"
##     p_mid_single <- "^[0-9]{4}$"
##     p_mid_multi <- "^[0-9]{4}-[0-9]{4}$"
##     p_open_last <- "^[0-9]{4}\\+$"
##     p_groups_multi <- "^([0-9]{4})-([0-9]{4})$"
##     ## must have at least one label
##     demcheck::err_is_positive_length(x = labels,
##                                      name = "labels")
##     ## only need to check one instance of each label
##     labels <- unique(labels)
##     n <- length(labels)
##     ## initial, non-definitive check that labels have expected format
##     is_open_first <- grepl(p_open_first, labels)
##     is_mid_single <- grepl(p_mid_single, labels)
##     is_mid_multi <- grepl(p_mid_multi, labels)
##     is_mid <- is_mid_single | is_mid_multi
##     is_open_last <- grepl(p_open_last, labels)
##     is_na <- is.na(labels)
##     is_invalid <- !(is_open_first | is_mid | is_open_last | is_na)
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" not a valid label for period measured in years",
##                         labels[[i_invalid]]))
##     is_single <- any(is_mid_single)
##     if (is_single && any(is_mid_multi))
##         return(gettextf("has mix of single-year labels [eg \"%s\"] and multi-year labels [eg \"%s\"]",
##                         labels[is_mid_single][[1L]], labels[is_mid_multi][[1L]]))
##     open_first <- any(is_open_first)
##     open_last <- any(is_open_last)
##     include_na <- any(is_na)
##     ## extract_years from middle labels, raising error if this cannot be done
##     if (is_single) {
##         years_mid <- labels_trimmed[is_mid_single]
##         is_invalid <- is.na(years_mid)
##     }
##     else {
##         years_mid <- matrix(nrow = 2L, ncol = sum(is_mid_multi))
##         years[1L, ] <- as.integer(sub(p_groups_multi, "\\1", labels[is_mid_multi]))
##         years[2L, ] <- as.integer(sub(p_groups_multi, "\\2", labels[is_mid_multi]))
##         is_invalid <- is.na(years[1L, ]) | is.na(years[2L, ])
##     }
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" not a valid label for period measured in years",
##                         labels[is_mid][[i_invalid]]))
##     ## for multiple years, check that upper value always greater than or equal to lower value
##     if (!is_single) {
##         is_invalid <- years[1L, ] > years[2L, ]
##         i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##         if (i_invalid > 0L)
##             return(gettextf("\"%s\" not a valid label for period measured in years ",
##                             labels[is_mid_multi][[i_invalid]]))
##     }
##     ## identify year for 'open_first'
##     if (open_first) {
##     years_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
##     is_invalid <- is.na(years_open_first)
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" not a valid label for period measured in years that is open on the left",
##                         labels[[i_invalid]]))
##     if (length(years_open_first) > 1L) {
##         is_invalid <- years_open_first[-1L] != years_open_first[1L]
##         i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##         if (i_invalid > 0L)
##             return(gettextf("inconsistent labels for period open on left : \"%s\" vs \"%s\"",
##                             labels[is_open_first][[1L]], labels[is_open_first][-1L][[i_invalid]]))
##     }
##     }
##     ## identify year for 'open_last'
##     if (open_last) {
##     years_open_last <- as.integer(sub("+$", "", labels[is_open_last]))
##     is_invalid <- is.na(years_open_last)
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" not a valid label for period measured in years that is open on the right",
##                         labels[[i_invalid]]))
##     if (length(years_open_last) > 1L) {
##         is_invalid <- years_open_last[-1L] != years_open_last[1L]
##         i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##         if (i_invalid > 0L)
##             return(gettextf("inconsistent labels for period open on left : \"%s\" vs \"%s\"",
##                             labels[is_open_last][[1L]], labels[is_open_last][-1L][[i_invalid]]))
##     }
##     ## check that 'open_left' aligns with
##     ## first element of 'years_mid'
##     if (is_open_first) {
##         if (is_single)
##             i_min_mid <- which.min(years_mid)
##         else
##             i_min_mid <- which.min(years_mid[1L, ])
##         if (years_open_first[[1L]] < min(years_mid))
##             return(gettextf("gap between periods \"%s\" and \"%s\"",
##                             labels[is_open_first][[1L]], labels[is_mid][[i_min_mid]]))
##         if (years_open_first[[1L]] > min(years_mid))
##             return(gettextf("label \"%s\" is open on the left, but label \"%s\" refers to an earlier year",
##                             labels[is_open_first][[1L]], labels[is_mid][[i_min_mid]]))
##     }
##     ## check that 'open_right' aligns with
##     ## last element of 'years_mid'
##     if (is_open_last) {
##         if (is_single)
##             i_max_mid <- which.max(years_mid)
##         else
##             i_max_mid <- which.max(years_mid[2L, ])
##         if (max(years_mid) < years_open_last[[1L]])
##             return(gettextf("gap between periods \"%s\" and \"%s\"",
##                             labels[is_mid][[i_max_mid]], labels[is_open_last][[1L]]))
##         if (max(years_mid) > years_open_last[[1L]])
##             return(gettextf("label \"%s\" is open on the right, but label \"%s\" refers to a later date",
##                             labels[is_open_last][[1L]], labels[is_mid][[i_mid_max]]))
##     }
##     ## check that 



    
##     is_invalid <- dates[is_open_last] != break_max
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##     labels_expected <- demprep::make_labels_period_quarter(break_min = break_min,
##                                                            break_max = break_max,
##                                                            open_first = open_first,
##                                                            open_last = open_last,
##                                                            include_na = include_na)
##     is_not_included <- !(labels_expected %in% labels)
##     i_not_included <- match(TRUE, is_not_included, nomatch = 0L)
##     has_gap <- i_not_included > 0L
##     if (has_gap && !gaps_ok)
##         return(gettextf("period labels have a gap : no label for period \"%s\"",
##                         labels_expected[[i_not_included]]))



##     list(years = years,
##          is_1_jan_or_label_first = is_1_jan_or_label_first,
##          open_first = open_first,
##          open_last = open_last,
##          include_na = include_na,
##          has_gap = has_gap)

## as.

##     list(break_min = break_min,
##          break_max = break_max,
##          open_first = open_first,
##          open_last = open_last,
##          include_na = include_na,
##          has_gap = has_gap)
## }


## HAS_TESTS
infer_dimscale_period_quarter <- function(labels, gaps_ok = FALSE) {
    ## regexp patterns
    p_open_first <- "^<[0-9]{4} Q[1-4]$"
    p_mid <- "^[0-9]{4} Q[1-4]$"
    p_open_last <- "^[0-9]{4} Q[1-4]\\+$"
    p_groups <- "^([0-9]{4}) Q([1-4])$"
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
        return(gettextf("\"%s\" not a valid label for period of one quarter",
                        labels[[i_invalid]]))
    open_first <- any(is_open_first)
    open_last <- any(is_open_last)
    include_na <- any(is_na)
    ## turn labels into dates, raising error if this cannot be done
    labels_trimmed <- labels
    labels_trimmed[is_open_first] <- sub("^<", "", labels_trimmed[is_open_first])
    labels_trimmed[is_open_last] <- sub("\\+$", "", labels_trimmed[is_open_last])
    year <- sub(p_groups, "\\1", labels_trimmed)
    quarter <- sub(p_groups, "\\2", labels_trimmed)
    month <- (as.integer(quarter) - 1L) * 3L + 1L
    dates <- rep(as.Date(NA), times = length(labels))
    dates[!is_na] <- as.Date(paste(year[!is_na], month[!is_na], 1L, sep = "-"))
    is_invalid <- is.na(dates) & !is_na
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid label for period of one quarter",
                        labels[[i_invalid]]))
    ## identify 'break_min' and 'break_max' and make sure
    ## that any open intervals are consistent with them
    i_min <- which.min(dates)
    i_max <- which.max(dates)
    break_min <- dates[[i_min]]
    break_max <- dates[[i_max]]
    if (!open_last)
        break_max <- seq.Date(from = break_max,
                              by = "quarter",
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
    labels_expected <- demprep::make_labels_period_quarter(break_min = break_min,
                                                           break_max = break_max,
                                                           open_first = open_first,
                                                           open_last = open_last,
                                                           include_na = include_na)
    is_not_included <- !(labels_expected %in% labels)
    i_not_included <- match(TRUE, is_not_included, nomatch = 0L)
    has_gap <- i_not_included > 0L
    if (has_gap && !gaps_ok)
        return(gettextf("period labels have a gap : no label for period \"%s\"",
                        labels_expected[[i_not_included]]))
    list(break_min = break_min,
         break_max = break_max,
         open_first = open_first,
         open_last = open_last,
         include_na = include_na,
         has_gap = has_gap)
}

## HAS_TESTS
infer_dimscale_period_month <- function(labels, gaps_ok = FALSE) {
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
        return(gettextf("\"%s\" not a valid label for period of one month",
                        labels[[i_invalid]]))
    open_first <- any(is_open_first)
    open_last <- any(is_open_last)
    include_na <- any(is_na)
    ## turn labels into dates, raising error if this cannot be done
    labels_trimmed <- labels
    labels_trimmed[is_open_first] <- sub("^<", "", labels_trimmed[is_open_first])
    labels_trimmed[is_open_last] <- sub("\\+$", "", labels_trimmed[is_open_last])
    dates <- rep(as.Date(NA), times = length(labels))
    dates[!is_na] <- suppressWarnings(as.Date(paste(labels_trimmed[!is_na], "01"),
                                              "%Y %b %d"))
    is_invalid <- is.na(dates) & !is_na
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid label for period of one month",
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
    labels_expected <- demprep::make_labels_period_month(break_min = break_min,
                                                         break_max = break_max,
                                                         open_first = open_first,
                                                         open_last = open_last,
                                                         include_na = include_na)
    is_not_included <- !(labels_expected %in% labels)
    i_not_included <- match(TRUE, is_not_included, nomatch = 0L)
    has_gap <- i_not_included > 0L
    if (has_gap && !gaps_ok)
        return(gettextf("period labels have a gap : no label for period \"%s\"",
                        labels_expected[[i_not_included]]))
    list(break_min = break_min,
         break_max = break_max,
         open_first = open_first,
         open_last = open_last,
         include_na = include_na,
         has_gap = has_gap)
}
