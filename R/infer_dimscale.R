
infer_dimscale_period <- function(labels) {
    ## regexp patterns
    p_open_first <- "^<([0-9]{4})$"
    p_single <- "^[0-9]{4}$"
    p_low_up <- "^([0-9]{4})-([0-9]{4})$"
    p_open_last <- "^([0-9]{4})\\+$"
    ## must have at least one label
    demcheck::err_is_positive_length(x = labels,
                                     name = "labels")
    ## only need to check one instance of each label, and
    ## sorting should work if labels have correct format
    labels <- unique(labels)
    labels <- sort(labels, na.last = TRUE)
    ## initial, non-definitive check that labels have correct format
    is_open_first <- grepl(p_open_first, labels)
    is_single <- grepl(p_single, labels)
    is_low_up <- grepl(p_low_up, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_na <- is.na(labels)
    is_invalid <- !(is_open_first | is_single | is_low_up | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid label for period measured in years",
                        labels[[i_invalid]]))
    ## summarise
    has_open_first <- any(is_open_first)
    has_single <- any(is_single)
    has_low_up <- any(is_low_up)
    has_open_last <- any(is_open_last)
    ## check that do not have mix of single and low_up labels
    if (has_single && has_low_up)
        return(gettextf("mix of single-year labels [eg \"%s\"] and lower-year, upper-year labels [eg \"%s\"]",
                        labels[is_single][[1L]], labels[is_low_up][[1L]]))
    ## check that at most one 'open_first' and at most one 'open_last'
    if (sum(is_open_first) > 1L)
        return(gettextf("two different labels for period open on left : \"%s\" and \"%s\"",
                        labels[is_open_first][[1L]], labels[is_open_first][[2L]]))
    if (sum(is_open_last) > 1L)
        return(gettextf("two different labels for period open on right : \"%s\" and \"%s\"",
                        labels[is_open_last][[1L]], labels[is_open_last][[2L]]))
    ## extract middle years
    if (has_mid) {
        if (has_single)
            years_mid <- as.integer(labels[is_mid])
        else {
            years_mid_low <- as.integer(sub(p_years_mid, "\\1", labels[is_mid]))
            years_mid_up <- as.integer(sub(p_years_mid, "\\2", labels[is_mid]))
        }
    }
    ## if has lower and upper, calculate widths, and check that valid
    if (has_low_up) {
        widths <- years_mid_up - years_mid_low
        is_invalid <- widths <= 0L
            i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
            if (i_invalid > 0L)
                return(gettextf("\"%s\" not a valid period : start year greater than or equal to end year",
                                labels[is_mid][[i_invalid]]))
    }
    ## if has lower and upper, check for overlap
    is_invalid <- years_mid_up[-n_max] > years_mid_low[-1L]
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("periods \"%s\" and \"%s\" overlap",
                        labels[is_mid][[i_invalid]],
                        labels[is_mid][[i_invalid + 1L]]))
    ## extract 'open_first' and 'open_last'
    if (has_open_first)
        year_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
    if (has_open_last)    
        year_open_last <- as.integer(sub("\\+$", "", labels[is_open_last]))
    ## if has 'open_first' and 'open_last', make sure they do not overlap
    if (has_open_first && has_open_last) {
        if (year_open_first > year_open_last)
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_open_last],
                            labels[is_open_first]))
    }
    ## if has mid, extract 'year_mid_min' and 'year_mid_max'
    if (has_mid) {
        if (has_single) {
            year_mid_min <- years_mid[[1L]]
            year_mid_max <- years_mid[[n_mid]]
        }
        else {
            year_mid_min <- years_mid_low[[1L]]
            year_mid_max <- years_mid_up[[n_mid]]
        }
    }
    ## if has mid, make sure does not overlap with 'open_first', 'open_last'
    if (has_mid && has_open_first)
        if (year_open_first > year_mid_min)
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_mid][[1L]],
                            labels[is_open_first]))
    if (has_mid && has_open_last) # 'has_open_last' implies 'label_year_start' is TRUE
        if (year_open_last <= year_mid_max)
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_mid][[n_max]],
                            labels[is_open_last]))
    
        
        



    
            ## widths equal
            if (n_mid > 1L)
                widths_equal <- all(widths[-1L] == widths[[1L]])
            else
                widths_equal <- TRUE
            ## min, max
            years_mid_min <- years_mid_low[[1L]]
            years_mid_max <- years_mid_up[[n_mid]]
        }
    }




, including 'year_mid_min', 'year_mid_max',
    ## and (for low-up) check years valid and whether widths equal
    n_mid <- sum(is_single | is_low_up)
    has_mid <- n_mid > 0L
    if (has_mid) {
        if (has_single) {
            years_mid <- as.integer(labels[is_mid])
            years_mid_min <- years_mid[[1L]]
            years_mid_max <- years_mid[[n_mid]]
        }
        else {
            years_mid_low <- as.integer(sub(p_years_mid, "\\1", labels[is_mid]))
            years_mid_up <- as.integer(sub(p_years_mid, "\\2", labels[is_mid]))
            widths <- years_mid_up - years_mid_low
            ## widths positive
            is_invalid <- widths <= 0L
            i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
            if (i_invalid > 0L)
                return(gettextf("\"%s\" not a valid period : start year greater than or equal to end year",
                                labels[is_mid][[i_invalid]]))
            ## no overlap
            is_invalid <- years_mid_up[-n_max] > years_mid_low[-1L]
            i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
            if (i_invalid > 0L)
                return(gettextf("periods \"%s\" and \"%s\" overlap",
                                labels[is_mid][[i_invalid]],
                                labels[is_mid][[i_invalid + 1L]]))
            ## widths equal
            if (n_mid > 1L)
                widths_equal <- all(widths[-1L] == widths[[1L]])
            else
                widths_equal <- TRUE
            ## min, max
            years_mid_min <- years_mid_low[[1L]]
            years_mid_max <- years_mid_up[[n_mid]]
        }
    }
    ## make sure 'open_first' and 'open_last' do not overlap
    
    ## get 'year_min', and also check for overlap
    if (has_mid) {
        if (open_first) {
            year_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
            if (year_open_first <= years_mid_min)
                year_min <- year_open_first
            else
                return(gettextf("periods \"%s\" and \"%s\" overlap",
                                labels[is_mid][[1L]],
                                labels[is_open_first]))
        }
        else
            year_min <- years_mid_min
    }
    else {
        if (open_first) {
            
    ## get 'year_max'
    if (open_last) {
        year_open_last <- as.integer(sub("\\+$", "", labels[is_open_last]))
        if (years_mid_max <= year_open_last)
            year_max <- year_open_last
        else
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_open_last],
                            labels[is_mid][[n_max]]))
    }
    else
        year_max <- years_mid_max



    
        year_min <- years_mid_min
    ## if is low_up, see if all same width
    if (has_low_up) {
        year_lower <- as.integer(sub(p_low_up, "\\1", labels[is_low_up]))
        year_upper <- as.integer(sub(p_low_up, "\\2", labels[is_low_up]))
        width <- year_upper - year_lower
        low_up_all_same <- all(width[-1L] == width[1L])
        if (low_all_same) {
            if (has_open_first) {
                year_first <- as.integer(sub(p_open_first, "\\1", label[is_open_first]))
                first_consistent <- low[[1L]] - year_first
            }
            else
                first_consistent <- TRUE
            if (has_open_last) {
                year_last <- as.integer(sub(p_open_last, "\\1", label[is_open_last]))
                last_consistent <- year_last - upper[[length(upper)]]
            }
            else
                last_consistent <- TRUE
            is_multi <- first_consistent && last_consistent
        }
        else
            is_multi <- FALSE
    }
    ## call specialised function
    if (has_single) {
        infer_dimscale_period_single(labels = labels,
                                     gaps_ok = gaps_ok)
    }
    else if (is_multi) {
        infer_dimscale_period_multi(labels = labels,
                                    gaps_ok = gaps_ok)
    }
    else {
        infer_dimscale_period_custom(labels = labels,
                                     gaps_ok = gaps_ok)
    }
}


## assume labels unique and sorted
infer_dimscale_period_single <- function(labels) {
    p_open_first <- "^<[0-9]{4}$"
    p_mid <- "^[0-9]{4}$"
    p_open_last <- "^[0-9]{4}\\+$"
    is_open_first <- grepl(p_open_first, labels)
    is_mid <- grepl(p_mid, labels)
    is_open_last <- grepl(p_open_last, labels)
    open_first <- any(is_open_first)
    open_last <- any(is_open_last)
    include_na <- any(is.na(labels))
    ## extract middle years
    years_mid <- as.integer(labels[is_mid])  # length >= 1
    n_mid <- length(years_mid)
    years_mid_min <- years[[1L]]
    years_mid_max <- years[[n_max]]
    ## get 'year_min'
    if (open_first) {
        year_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
        if (year_open_first <= years_mid_min)
            year_min <- year_open_first
        else
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_mid][[1L]],
                            labels[is_open_first]))
    }
    else
        year_min <- years_mid_min
    ## get 'year_max'
    if (open_last) {
        year_open_last <- as.integer(sub("\\+$", "", labels[is_open_last]))
        if (years_mid_max <= year_open_last)
            year_max <- year_open_last
        else
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_open_last],
                            labels[is_mid][[n_max]]))
    }
    else
        year_max <- years_mid_max
    ## return description of complete dimscale
    new("PeriodSingle",
        year_min = year_min,
        year_max = year_max,
        open_first = open_first,
        open_last = open_last,
        include_na = include_na)
}


## Assume that labels unique and unique, that there is
## at least one lower-upper pair, and that
## all pairs (including pairs formed by open intervals)
## have same width
infer_dimscale_period_multi <- function(labels, gaps_ok) {
    p_open_first <- "^<[0-9]{4}$"
    p_mid <- "^([0-9]{4})-([0-9]{4})$"
    p_open_last <- "^[0-9]{4}\\+$"
    is_open_first <- grepl(p_open_first, labels)
    is_mid <- grepl(p_mid, labels)
    is_open_last <- grepl(p_open_last, labels)
    open_first <- any(is_open_first)
    open_last <- any(is_open_last)
    include_na <- any(is.na(labels))
    ## extract middle years
    years_lower <- as.integer(sub(p_years_mid, "\\1", labels[is_mid]))  # length >= 1
    years_upper <- as.integer(sub(p_years_mid, "\\2", labels[is_mid]))  # length >= 1
    width <- years_upper[[1L]] - years_lower[[1L]]
    if (width <= 0L)
        return(gettextf("\"%s\" not a valid period : start year greater than or equal to end year",
                        labels[is_mid][[1L]]))
    n_mid <- length(years_lower)
    year_mid_min <- years_lower[[1L]]
    year_mid_max <- years_upper[[n_mid]]
    ## get 'year_min'
    if (open_first) {
        year_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
        if (year_open_first <= years_mid_min)
            year_min <- year_open_first
        else
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_mid][[1L]],
                            labels[is_open_first]))
    }
    else
        year_min <- years_mid_min
    ## get 'year_min'
    if (open_first) {
        year_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
        if (year_open_first <= years_mid_min)
            year_min <- year_open_first
        else
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_mid][[1L]],
                            labels[is_open_first]))
    }
    else
        year_min <- years_mid_min
    ## get 'year_max'
    if (open_last) {
        year_open_last <- as.integer(sub("\\+$", "", labels[is_open_last]))
        if (years_mid_max <= year_open_last)
            year_max <- year_open_last
        else
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_open_last],
                            labels[is_mid][[n_max]]))
    }
    else
        year_max <- years_mid_max
    ## return description of complete dimscale
    list(years = years,
         open_first = open_first,
         open_last = open_last,
         include_na = include_na,
         has_gap = has_gap)
}



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
        return(gettextf("label \"%s\" is for interval that is open on left, but label \"%s\" refers to earlier date",
                        labels[is_open_first][[i_invalid]],
                        labels[[i_min]]))
    is_invalid <- dates[is_open_last] != break_max
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("label \"%s\" is for interval that is open on right, but label \"%s\" refers to later date",
                        labels[is_open_last][[i_invalid]],
                        labels[[i_max]]))
    ## make expected labels, and check whether all these labels in fact occur
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
    ## return description of dimscale
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
        return(gettextf("label \"%s\" is for interval that is open on left, but label \"%s\" refers to earlier date",
                        labels[is_open_first][[i_invalid]],
                        labels[[i_min]]))
    is_invalid <- dates[is_open_last] != break_max
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("label \"%s\" is for interval that is open on right, but label \"%s\" refers to later date",
                        labels[is_open_last][[i_invalid]],
                        labels[[i_max]]))
    ## make expected labels, and check whether all these labels in fact occur
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
    ## return description of dimscale
    list(break_min = break_min,
         break_max = break_max,
         open_first = open_first,
         open_last = open_last,
         include_na = include_na,
         has_gap = has_gap)
}
