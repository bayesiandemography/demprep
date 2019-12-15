
infer_lab_categories <- function(labels) {
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## check for blanks
    is_blank <- !nzchar(labels)
    if (any(is_blank))
        return(gettextf("'%s' has blanks",
                        "labels"))
    ## check for NA
    has_na <- any(is.na(labels))
    ## return result
    LabCategories(labels = labels,
                  include_na = has_na)
}


infer_lab_triangles <- function(labels) {
    valid_labels <- c("Lower", "Upper")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## check for blanks
    is_blank <- !nzchar(labels)
    if (any(is_blank))
        return(gettextf("'%s' has blanks",
                        "labels"))
    ## identify NAs
    is_na <- is.na(labels)
    has_na <- any(is.na(labels))
    ## check for invalid values
    is_invalid <- !is_na & !(labels %in% valid_labels)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L) {
        return(gettextf("\"%s\" not a valid label for triangles",
                        labels[[i_invalid]]))
    }
    ## return result
    LabTriangles(include_na = has_na)
}


infer_lab_integers <- function(labels) {
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## must have at least one non-NA label
    ## (otherwise use different Label class)
    is_na <- is.na(labels)
    if (identical(sum(!is_na), 0L))
        return(gettextf("'%s' has no non-NA elements",
                        "labels"))
    has_na <- any(is_na)
    ## check for integer
    labels_int <- tryCatch(error = function(e) e,
                           demcheck::err_tdy_integer_vector(x = labels,
                                                            name = "labels"))
    if (methods::is(labels_int, "error"))
        return(val$message)
    int_min <- min(labels_int, na.rm = TRUE)
    int_max <- max(labels_int, na.rm = TRUE)
    LabIntegers(int_min = int_min,
                int_max = int_max,
                include_na = has_na)
}

infer_lab_grouped_int_enumerations <- function(labels) {
    ## regexp patterns
    p_open_first <- "^<(-?[0-9]+)$"
    p_single <- "^-?[0-9]+$"
    p_low_up <- "^(-?[0-9]+)-([0-9]+)$"
    p_open_last <- "^(-?[0-9]+)\\+$"
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## sorting should work if labels have correct format
    labels <- sort(labels)
    ## initial, non-definitive check that labels have correct format
    is_open_first <- grepl(p_open_first, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_single <- grepl(p_single, labels)
    is_low_up <- grepl(p_low_up, labels)
    is_mid <- is_single | is_low_up
    is_na <- is.na(labels)
    is_invalid <- !(is_open_first | is_single | is_low_up | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid label for period measured in years",
                        labels[[i_invalid]]))
    ## summarise
    has_open_first <- any(is_open_first)
    has_open_last <- any(is_open_last)
    has_single <- any(is_single)
    has_low_up <- any(is_low_up)
    has_mid <- any(is_mid)
    has_na <- any(is_na)
    n_mid <- sum(is_mid)
    ## check that at most one 'open_first', and at most one 'open_last'
    if (sum(is_open_first) > 1L)
        return(gettextf("two different labels for period open on left : \"%s\" and \"%s\"",
                        labels[is_open_first][[1L]], labels[is_open_first][[2L]]))
    if (sum(is_open_last) > 1L)
        return(gettextf("two different labels for period open on right : \"%s\" and \"%s\"",
                        labels[is_open_last][[1L]], labels[is_open_last][[2L]]))
    ## process middle years, if present
    if (has_single)
        years_single <- as.integer(labels[is_single])
    if (has_low_up) {
        years_low <- as.integer(sub(p_years_mid, "\\1", labels[is_low_up]))
        years_up <- as.integer(sub(p_years_mid, "\\2", labels[is_low_up]))
    }
    ## if has lower and upper, calculate widths, and check that valid
    if (has_low_up) {
        widths <- years_up - years_low
        is_invalid <- widths <= 0L
        i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
        if (i_invalid > 0L)
            return(gettextf("\"%s\" not a valid label for an enumeration",
                            labels[is_low_up][[i_invalid]]))
    }
    ## extract 'year_open_first' and 'year_open_last' if present, and check for overlap
    if (has_open_first)
        year_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
    if (has_open_last)
        year_open_last <- as.integer(sub("\\+$", "", labels[is_open_last]))
    if (has_open_first && has_open_last) {
        if(year_open_first > year_open_last)
            return(gettextf("intervals implied by labels \"%s\" and \"%s\" overlap",
                            labels[is_open_first][[1L]], labels[is_open_last][[1L]]))
    }
    ## obtain breaks for mid, and check for overlap
    if (has_mid) {
        breaks_mid_low <- integer(length = n_mid)
        breaks_mid_up <- integer(length = n_mid)
        breaks_mid_low[is_single] <- years_single
        breaks_mid_up[is_single] <- years_single + 1L
        breaks_mid_low[is_low_up] <- years_low
        breaks_mid_up[is_low_up] <- years_up + 1L
        ## check mid for overlap
        is_overlap <- breaks_mid_up[-n_mid] > breaks_mid_low[-1L]
        i_overlap <- match(TRUE, is_overlap, nomatch = 0L)
        if (i_overlap > 0L) {
            gettextf("intervals implied by labels \"%s\" and \"%s\" overlap",
                     labels[is_mid][[i_overlap]],
                     labels[is_mid][[i_overlap + 1L]])
        }
    }
    ## check for overlap between mid and open_first, open_last
    if (has_mid) {
        if (has_open_first) {
            if(year_open_first > breaks_mid_low[[1L]])
                return(gettextf("intervals implied by labels \"%s\" and \"%s\" overlap",
                                labels[is_open_first], labels[is_mid][[1L]]))
        }
        if (has_open_last) {
            if(breaks_mid_up[[n]] > year_open_last)
                return(gettextf("intervals implied by labels \"%s\" and \"%s\" overlap",
                                labels[is_mid][[n_mid]], labels[is_open_last]))
        }
    }
    ## assemble breaks
    breaks <- c(breaks_mid_low, breaks_mid_up)
    if (has_open_first)
        breaks <- c(breaks, year_open_first)
    if (has_open_last)
        breaks <- c(breaks_year_open_last)
    breaks <- sort(unique(breaks))
    ## return "Label" object
    LabGroupedIntEnumeration(breaks = breaks,
                             open_first = has_open_first,
                             open_last = has_open_last,
                             include_na = has_na)
}

## ## HAS_TESTS
## infer_dimscale_period_quarter <- function(labels, gaps_ok = FALSE) {
##     ## regexp patterns
##     p_open_first <- "^<([0-9]{4}) Q([1-4])$"
##     p_mid <- "^([0-9]{4}) Q([1-4])$"
##     ## must have at least one label
##     demcheck::err_is_positive_length(x = labels,
##                                      name = "labels")
##     ## only need to process one instance of each label, and
##     ## sorting should work if labels have correct format
##     labels <- unique(labels)
##     labels <- sort(labels, na.last = TRUE)
##     ## initial, non-definitive check that labels have expected format
##     is_open_first <- grepl(p_open_first, labels)
##     is_mid <- grepl(p_mid, labels)
##     is_na <- is.na(labels)
##     is_invalid <- !(is_open_first | is_mid | is_na)
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" not a valid label for period of one quarter",
##                         labels[[i_invalid]]))
##     ## summarise
##     has_open_first <- any(is_open_first)
##     has_mid <- any(is_mid)
##     has_na <- any(is_na)
##     n_mid <- sum(has_mid)
##     ## check that at most one 'open_first'
##     if (sum(is_open_first) > 1L)
##         return(gettextf("two different labels for period open on left : \"%s\" and \"%s\"",
##                         labels[is_open_first][[1L]], labels[is_open_first][[2L]]))
##     ## extract 'dates_mid', if present
##     years_mid <- sub(p_mid, "\\1", labels[is_mid])
##     quarters_mid <- sub(p_mid, "\\2", labels[is_mid])
##     months_mid <- (as.integer(quarters_mid) - 1L) * 3L + 1L
##     dates_mid <- paste(years_mid, months_mid, 1, sep = "-")
##     dates_mid <- as.Date(dates_mid)
##     ## extract 'date_open_first', if present
##     if (has_open_first) {
##         year_open_first <- sub(p_open_first, "\\1", labels[is_open_first])
##         quarter_open_first <- sub(p_open_first, "\\2", labels[is_open_first])
##         month_open_first <- (as.integer(quarter) - 1L) * 3L + 1L
##         date_open_first <- paste(year_open_first, month_open_first, 1, sep = "-")
##         date_open_first <- as.Date(date_open_first)
##     }
##     ## extract 'date_mid_min' and 'date_min_max', if present
##     if (has_mid) {
##         date_mid_min <- min(dates_mid)
##         date_mid_max <- max(dates_mid)
##     }
##     ## make sure 'open_first' does not overlap with mid   
##     if (has_open_first && has_mid) {
##         if (date_open_first > date_mid_min)
##             return(gettextf("periods \"%s\" and \"%s\" overlap",
##                             labels[is_mid][[1L]],
##                             labels[is_open_first]))
##     }
##     ## return "PeriodQuarter" object
##     break_min <- if (has_open_first) date_open_first else date_mid_min
##     new("PeriodQuarter",
##         include_na = has_na,
##         open_first = has_open_first,
##         break_min = break_min,
##         break_max = date_mid_max)
## }





## ## HAS_TESTS
## infer_dimscale_period_month <- function(labels, gaps_ok = FALSE) {
##     ## regexp patterns
##     p_open_first <- "^<([0-9]{4}) ([A-z]{3})$"
##     p_mid <- "^([0-9]{4}) ([A-z]{3})$"
##     ## must have at least one label
##     demcheck::err_is_positive_length(x = labels,
##                                      name = "labels")
##     ## only need to process one instance of each label, and
##     ## sorting should work if labels have correct format
##     labels <- unique(labels)
##     labels <- sort(labels, na.last = TRUE)
##     ## initial, non-definitive check that labels have expected format
##     is_open_first <- grepl(p_open_first, labels)
##     is_mid <- grepl(p_mid, labels)
##     is_na <- is.na(labels)
##     is_invalid <- !(is_open_first | is_mid | is_na)
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" not a valid label for period of one month",
##                         labels[[i_invalid]]))
##     ## summarise
##     has_open_first <- any(is_open_first)
##     has_mid <- any(is_mid)
##     has_na <- any(is_na)
##     n_mid <- sum(is_mid)
##     ## check that at most one 'open_first'
##     if (sum(is_open_first) > 1L)
##         return(gettextf("two different labels for period open on left : \"%s\" and \"%s\"",
##                         labels[is_open_first][[1L]], labels[is_open_first][[2L]]))
##     ## extract 'dates_mid', if present
##     years_mid <- sub(p_mid, "\\1", labels[is_mid])
##     months_mid <- sub(p_mid, "\\1", labels[is_mid])
##     dates_mid <- paste(years_mid, months_mid, 1, sep = "-")
##     dates_mid <- as.Date(dates_mid, format = "%Y %b %d")
##     ## extract 'date_open_first', if present
##     if (has_open_first) {
##         year_open_first <- sub(p_open_first, "\\1", labels[is_open_first])
##         month_open_first <- sub(p_open_first, "\\2", labels[is_open_first])
##         date_open_first <- paste(year_open_first, month_open_first, 1, sep = "-")
##         date_open_first <- as.Date(date_open_first, format = "%Y %b %d")
##     }
##     ## extract 'date_mid_min' and 'date_min_max', if present
##     if (has_mid) {
##         date_mid_min <- min(dates_mid)
##         date_mid_max <- max(dates_mid)
##     }
##     ## make sure 'open_first' does not overlap with mid   
##     if (has_open_first && has_mid) {
##         if (date_open_first > date_mid_min)
##             return(gettextf("periods \"%s\" and \"%s\" overlap",
##                             labels[is_mid][[1L]],
##                             labels[is_open_first]))
##     }
##     ## return "PeriodMonth" object
##     break_min <- if (has_open_first) date_open_first else date_mid_min
##     new("PeriodQuarter",
##         include_na = has_na,
##         open_first = has_open_first,
##         break_min = break_min,
##         break_max = date_mid_max)
## }


## ## HAS_TESTS
## infer_dimscale_period_month <- function(labels, gaps_ok = FALSE) {
##     ## regexp patterns
##     p_open_first <- "^<[0-9]{4} [A-z]{3}$"
##     p_mid <- "^[0-9]{4} [A-z]{3}$"
##     p_open_last <- "^[0-9]{4} [A-z]{3}\\+$"
##     ## must have at least one label
##     demcheck::err_is_positive_length(x = labels,
##                                      name = "labels")
##     ## only need to process one instance of each label
##     labels <- unique(labels)
##     ## initial, non-definitive check that labels have expected format
##     is_open_first <- grepl(p_open_first, labels)
##     is_mid <- grepl(p_mid, labels)
##     is_open_last <- grepl(p_open_last, labels)
##     is_na <- is.na(labels)
##     is_invalid <- !(is_open_first | is_mid | is_open_last | is_na)
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" not a valid label for period of one month",
##                         labels[[i_invalid]]))
##     open_first <- any(is_open_first)
##     open_last <- any(is_open_last)
##     include_na <- any(is_na)
##     ## turn labels into dates, raising error if this cannot be done
##     labels_trimmed <- labels
##     labels_trimmed[is_open_first] <- sub("^<", "", labels_trimmed[is_open_first])
##     labels_trimmed[is_open_last] <- sub("\\+$", "", labels_trimmed[is_open_last])
##     dates <- rep(as.Date(NA), times = length(labels))
##     dates[!is_na] <- suppressWarnings(as.Date(paste(labels_trimmed[!is_na], "01"),
##                                               "%Y %b %d"))
##     is_invalid <- is.na(dates) & !is_na
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("\"%s\" not a valid label for period of one month",
##                         labels[[i_invalid]]))
##     ## identify 'break_min' and 'break_max' and make sure
##     ## that any open intervals are consistent with them
##     i_min <- which.min(dates)
##     i_max <- which.max(dates)
##     break_min <- dates[[i_min]]
##     break_max <- dates[[i_max]]
##     if (!open_last)
##         break_max <- seq.Date(from = break_max,
##                               by = "month",
##                               length.out = 2L)[[2L]]
##     is_invalid <- dates[is_open_first] != break_min
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("label \"%s\" is for interval that is open on left, but label \"%s\" refers to earlier date",
##                         labels[is_open_first][[i_invalid]],
##                         labels[[i_min]]))
##     is_invalid <- dates[is_open_last] != break_max
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     if (i_invalid > 0L)
##         return(gettextf("label \"%s\" is for interval that is open on right, but label \"%s\" refers to later date",
##                         labels[is_open_last][[i_invalid]],
##                         labels[[i_max]]))
##     ## make expected labels, and check whether all these labels in fact occur
##     labels_expected <- demprep::make_labels_period_month(break_min = break_min,
##                                                          break_max = break_max,
##                                                          open_first = open_first,
##                                                          open_last = open_last,
##                                                          include_na = include_na)
##     is_not_included <- !(labels_expected %in% labels)
##     i_not_included <- match(TRUE, is_not_included, nomatch = 0L)
##     has_gap <- i_not_included > 0L
##     if (has_gap && !gaps_ok)
##         return(gettextf("period labels have a gap : no label for period \"%s\"",
##                         labels_expected[[i_not_included]]))
##     ## return description of dimscale
##     list(break_min = break_min,
##          break_max = break_max,
##          open_first = open_first,
##          open_last = open_last,
##          include_na = include_na,
##          has_gap = has_gap)
## }
