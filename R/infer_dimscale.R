
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
##     is_open_last <- grepl(p_open_last, labels)
##     is_na <- is.na(labels)
##     is_invalid <- !(is_open_first | is_mid_single | is_mid_multi | is_open_last | is_na)
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" is not a valid label for a period measured in years",
##                         labels[[i_invalid]]))
##     open_first <- any(is_open_first)
##     open_last <- any(is_open_last)
##     include_na <- any(is_na)
##     ## extract_years, raising error if this cannot be done
##     labels_trimmed <- labels
##     labels_trimmed[is_open_first] <- sub("^<", "", labels_trimmed[is_open_first])
##     labels_trimmed[is_open_last] <- sub("\\+$", "", labels_trimmed[is_open_last])
##     perm_labels <- order(labels_trimmed)
##     labels_trimmed <- labels_trimmed[order]
    
##     years <- vector(mode = "list", length = n)
##     is_single_num <- is_open_first | is_mid_single | is_open_last
##     years[is_single_num] <- as.list(as.integer(labels_trimmed))
##     years_first <- sub(p_groups_multi, "\\1", labels_trimmed[is_mid_multi])
##     years_last <- sub(p_groups_multi, "\\2", labels_trimmed[is_mid_multi])
##     years_multi <- mapply(c, years_first, years_last, SIMPLIFY = FALSE)
##     years_multi <- lapply(years_multi, as.integer)
##     years[is_mid_multi] <- years_multi
##     years_has_na <- lapply(years, function(x) any(is.na(x)))
##     is_invalid <- years_has_na & !is_na
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" is not a valid label for a period of one quarter",
##                         labels[[i_invalid]]))
##     ## if possible, determine whether periods begin on 1 Jan or
##     ## if single years are labelled using the start year
##     if (is_open_last) {
##         is_1_jan_or_label_first <- TRUE # open_right only TRUE if label_year_start TRUE
##     }
##     else {
##         is_single_then_multi <- is_mid_single[-n] & is_mid_multi[-1L]
##         is_multi_then_single <- is_mid_multi[-n] & is_mid_single[-1L]
##         if (any(is_single_then_multi)) {
##             i_single_then_multi <- match(TRUE, is_single_then_multi) 
##             year_single <- years[[i_single_then_multi]]
##             year_multi <- years[[i_single_then_multi + 1L]][[1L]]
##             is_1_jan_or_label_first <- year_single < 
##         }
##         else if (any(is_multi_then_single)) {

        
##         }
##         else
##             is_1_jan_or_label_first <- NA
##     }
    
##     ## extract_years
    
##     ## turn labels into dates, raising error if this cannot be done
##     labels_trimmed <- labels
##     labels_trimmed[is_open_first] <- sub("^<", "", labels_trimmed[is_open_first])
##     labels_trimmed[is_open_last] <- sub("\\+$", "", labels_trimmed[is_open_last])
##     year <- sub(p_groups, "\\1", labels_trimmed)
##     quarter <- sub(p_groups, "\\2", labels_trimmed)
##     month <- (as.integer(quarter) - 1L) * 3L + 1L
##     dates <- rep(as.Date(NA), times = length(labels))
##     dates[!is_na] <- as.Date(paste(year[!is_na], month[!is_na], 1L, sep = "-"))
##     is_invalid <- is.na(dates) & !is_na
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" is not a valid label for a period of one quarter",
##                         labels[[i_invalid]]))
##     ## identify 'break_min' and 'break_max' and make sure
##     ## that any open intervals are consistent with them
##     i_min <- which.min(dates)
##     i_max <- which.max(dates)
##     break_min <- dates[[i_min]]
##     break_max <- dates[[i_max]]
##     if (!open_last)
##         break_max <- seq.Date(from = break_max,
##                               by = "quarter",
##                               length.out = 2L)[[2L]]
##     is_invalid <- dates[is_open_first] != break_min
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("label \"%s\" is open on the left, but label \"%s\" refers to an earlier date",
##                         labels[is_open_first][[i_invalid]],
##                         labels[[i_min]]))
##     is_invalid <- dates[is_open_last] != break_max
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("label \"%s\" is open on the right, but label \"%s\" refers to a later date",
##                         labels[is_open_last][[i_invalid]],
##                         labels[[i_max]]))
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
        return(gettextf("\"%s\" is not a valid label for a period of one quarter",
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
        return(gettextf("\"%s\" is not a valid label for a period of one quarter",
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
        return(gettextf("\"%s\" is not a valid label for a period of one month",
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
