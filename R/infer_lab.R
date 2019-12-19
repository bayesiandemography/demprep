

## Functions for dimtypes -----------------------------------------------------

## dimtypes "pool", "triangle", and "quantiles" all have
## their own unique 'infer_lab' functions

infer_lab_attribute <- function(labels) {
    funs <- list(infer_lab_integers,
                 inter_lab_enumerations,
                 infer_lab_categories)
    for (fun in funs) {
        val <- fun(labels)
        if (methods::is(val, "Labels"))
            return(val)
    }
    stop(gettext("could not process labels"))
}

infer_lab_iterations <- function(labels) {
    funs <- list(infer_lab_integers,
                 infer_lab_categories)
    for (fun in funs) {
        val <- fun(labels)
        if (methods::is(val, "Labels"))
            return(val)
    }
    stop(gettext("could not process labels"))
}



infer_lab_age <- function(labels) {
    funs <- list(infer_lab_integers,
                 infer_lab_grouped_int_enumerations,
                 infer_lab_durations_quarters,
                 infer_lab_durations_months,
                 infer_lab_durations_categories)
    for (fun in funs) {
        val <- fun(labels)
        if (methods::is(val, "Labels"))
            return(val)
    }
    stop(gettext("could not process labels"))
}

infer_lab_time <- function(labels) {
    funs <- list(infer_lab_integers,
                 infer_lab_grouped_int_endpoints,
                 infer_lab_calendar_quarters,
                 infer_lab_calendar_months,
                 infer_lab_durations_categories)
    for (fun in funs) {
        val <- fun(labels)
        if (methods::is(val, "Labels"))
            return(val)
    }
    stop(gettext("could not process labels"))
}

infer_lab_cohort <- function(labels) {
    funs <- list(infer_lab_integers,
                 infer_lab_grouped_int_endpoints,
                 infer_lab_calendar_quarters,
                 infer_lab_calendar_months,
                 infer_lab_durations_categories)
    for (fun in funs) {
        val <- fun(labels)
        if (methods::is(val, "Labels"))
            return(val)
    }
    stop(gettext("could not process labels"))
}




    
    
    


## Functions for individual "Label" classes -----------------------------------

## HAS_TESTS
infer_lab_categories <- function(labels) {
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## see if has NA, and remove if present
    is_na <- is.na(labels)
    labels <- labels[!is_na]
    has_na <- any(is_na)
    ## return result
    LabCategories(labels = labels,
                  include_na = has_na)
}

## HAS_TESTS
infer_lab_triangles <- function(labels) {
    valid_labels <- c("Lower", "Upper")
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## see if has NA, and remove if present
    is_na <- is.na(labels)
    labels <- labels[!is_na]
    has_na <- any(is_na)
    ## check for invalid values
    is_invalid <- !(labels %in% valid_labels)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L) {
        return(gettextf("\"%s\" not a valid label for triangles",
                        labels[[i_invalid]]))
    }
    ## return result
    LabTriangles(include_na = has_na)
}

## HAS_TESTS
infer_lab_pool <- function(labels) {
    valid_labels <- c("Ins", "Outs")
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## see if has NA, and remove if present
    is_na <- is.na(labels)
    labels <- labels[!is_na]
    has_na <- any(is_na)
    ## check for invalid values
    is_invalid <- !(labels %in% valid_labels)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L) {
        return(gettextf("\"%s\" not a valid label for pool",
                        labels[[i_invalid]]))
    }
    ## return result
    LabPool(include_na = has_na)
}

## HAS_TESTS
infer_lab_quantiles <- function(labels) {
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## see if has NA, and remove if present
    is_na <- is.na(labels)
    labels <- labels[!is_na]
    has_na <- any(is_na)
    ## check for invalid values
    val <- demcheck::chk_is_valid_quantile(x = labels,
                                           name = "labels")
    if (!isTRUE(val))
        return(val)
    ## sort labels
    labels <- sort_quantiles(labels)
    ## return result
    LabQuantiles(labels = labels,
                 include_na = has_na)
}

## HAS_TESTS
infer_lab_integers <- function(labels) {
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## must have at least one non-NA label
    ## (otherwise use different Label class)
    is_na <- is.na(labels)
    if (identical(sum(!is_na), 0L))
        return(gettextf("'%s' has no non-NA elements",
                        "labels"))
    has_na <- any(is_na)
    ## remove NAs
    labels <- labels[!is_na]
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

## HAS_TESTS
infer_lab_grouped_int_enumerations <- function(labels) {
    ## regexp patterns
    p_open_first <- "^<(-?[0-9]+)$"
    p_single <- "^(-?[0-9]+)$"
    p_low_up <- "^(-?[0-9]+)-(-?[0-9]+)$"
    p_open_last <- "^(-?[0-9]+)\\+$"
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## sort labels
    labels <- sort_intervals(labels)
    ## must have at least one non-NA label
    ## (otherwise use different Label class)
    is_na <- is.na(labels)
    if (identical(sum(!is_na), 0L))
        return(gettextf("'%s' has no non-NA elements",
                        "labels"))
    ## initial, non-definitive check that labels have correct format
    is_open_first <- grepl(p_open_first, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_single <- grepl(p_single, labels)
    is_low_up <- grepl(p_low_up, labels)
    is_mid <- is_single | is_low_up
    is_invalid <- !(is_open_first | is_mid | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid enumeration label",
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
        return(gettextf("two different labels for interval open on left : \"%s\" and \"%s\"",
                        labels[is_open_first][[1L]], labels[is_open_first][[2L]]))
    if (sum(is_open_last) > 1L)
        return(gettextf("two different labels for interval open on right : \"%s\" and \"%s\"",
                        labels[is_open_last][[1L]], labels[is_open_last][[2L]]))
    ## process middle years
    years_single <- as.integer(labels[is_single])
    years_low <- as.integer(sub(p_low_up, "\\1", labels[is_low_up]))
    years_up <- as.integer(sub(p_low_up, "\\2", labels[is_low_up]))
    ## calculate widths, and check that valid
    widths <- years_up - years_low
    is_invalid <- widths <= 0L
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid enumeration label",
                        labels[is_low_up][[i_invalid]]))
    ## extract 'year_open_first' and 'year_open_last' if present, and check for overlap
    year_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
    year_open_last <- as.integer(sub("\\+$", "", labels[is_open_last]))
    if (has_open_first && has_open_last) {
        if(year_open_first > year_open_last)
            return(gettextf("intervals defined by labels \"%s\" and \"%s\" overlap",
                            labels[is_open_first][[1L]], labels[is_open_last][[1L]]))
    }
    ## obtain breaks for mid, and check for overlap
    breaks_mid_low <- integer(length = n_mid)
    breaks_mid_up <- integer(length = n_mid)
    is_single_mid <- is_single[is_mid]
    is_low_up_mid <- is_low_up[is_mid]
    breaks_mid_low[is_single_mid] <- years_single
    breaks_mid_up[is_single_mid] <- years_single + 1L
    breaks_mid_low[is_low_up_mid] <- years_low
    breaks_mid_up[is_low_up_mid] <- years_up + 1L
    ## check mid for overlap
    is_overlap <- breaks_mid_up[-n_mid] > breaks_mid_low[-1L]
    i_overlap <- match(TRUE, is_overlap, nomatch = 0L)
    if (i_overlap > 0L) {
        return(gettextf("intervals defined by labels \"%s\" and \"%s\" overlap",
                        labels[is_mid][[i_overlap]],
                        labels[is_mid][[i_overlap + 1L]]))
    }
    ## check for overlap between mid and open_first, open_last
    if (has_mid) {
        if (has_open_first) {
            if(year_open_first > breaks_mid_low[[1L]])
                return(gettextf("intervals defined by labels \"%s\" and \"%s\" overlap",
                                labels[is_open_first], labels[is_mid][[1L]]))
        }
        if (has_open_last) {
            if(breaks_mid_up[[n_mid]] > year_open_last)
                return(gettextf("intervals defined by labels \"%s\" and \"%s\" overlap",
                                labels[is_mid][[n_mid]], labels[is_open_last]))
        }
    }
    ## assemble breaks
    breaks <- integer()
    if (has_mid)
        breaks <- c(breaks, breaks_mid_low, breaks_mid_up)
    if (has_open_first)
        breaks <- c(breaks, year_open_first)
    if (has_open_last)
        breaks <- c(breaks, year_open_last)
    breaks <- sort(unique(breaks))
    ## return "Label" object
    LabGroupedIntEnumerations(breaks = breaks,
                              open_first = has_open_first,
                              open_last = has_open_last,
                              include_na = has_na)
}


infer_lab_grouped_int_endpoints <- function(labels) {
    ## regexp patterns
    p_open_first <- "^<(-?[0-9]+)$"
    p_mid <- "^(-?[0-9]+)-(-?[0-9]+)$"
    p_open_last <- "^(-?[0-9]+)\\+$"
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## sort labels
    labels <- sort_intervals(labels)
    ## must have at least one non-NA label
    ## (otherwise use different Label class)
    is_na <- is.na(labels)
    if (identical(sum(!is_na), 0L))
        return(gettextf("'%s' has no non-NA elements",
                        "labels"))
    ## initial, non-definitive check that labels have correct format
    is_open_first <- grepl(p_open_first, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_mid <- grepl(p_mid, labels)
    is_na <- is.na(labels)
    is_invalid <- !(is_open_first | is_mid | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid endpoints label",
                        labels[[i_invalid]]))
    ## summarise
    has_open_first <- any(is_open_first)
    has_open_last <- any(is_open_last)
    has_mid <- any(is_mid)
    has_na <- any(is_na)
    n_mid <- sum(is_mid)
    ## check that at most one 'open_first', and at most one 'open_last'
    if (sum(is_open_first) > 1L)
        return(gettextf("two different labels for interval open on left : \"%s\" and \"%s\"",
                        labels[is_open_first][[1L]], labels[is_open_first][[2L]]))
    if (sum(is_open_last) > 1L)
        return(gettextf("two different labels for interval open on right : \"%s\" and \"%s\"",
                        labels[is_open_last][[1L]], labels[is_open_last][[2L]]))
    ## process middle years
    years_low <- as.integer(sub(p_mid, "\\1", labels[is_mid]))
    years_up <- as.integer(sub(p_mid, "\\2", labels[is_mid]))
    ## calculate widths, and check that valid
    widths <- years_up - years_low
    is_invalid <- widths <= 0L
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid endpoints label",
                        labels[is_mid][[i_invalid]]))
    ## extract 'year_open_first' and 'year_open_last' if present, and check for overlap
    year_open_first <- as.integer(sub("^<", "", labels[is_open_first]))
    year_open_last <- as.integer(sub("\\+$", "", labels[is_open_last]))
    if (has_open_first && has_open_last) {
        if(year_open_first > year_open_last)
            return(gettextf("intervals defined by labels \"%s\" and \"%s\" overlap",
                            labels[is_open_first][[1L]], labels[is_open_last][[1L]]))
    }
    ## check for overlap in mid
    if (has_mid) {
        is_overlap <- years_up[-n_mid] > years_low[-1L]
        i_overlap <- match(TRUE, is_overlap, nomatch = 0L)
        if (i_overlap > 0L) {
            return(gettextf("intervals defined by labels \"%s\" and \"%s\" overlap",
                            labels[is_mid][[i_overlap]],
                            labels[is_mid][[i_overlap + 1L]]))
        }
    }
    ## check for overlap between mid and open_first, open_last
    if (has_mid) {
        if (has_open_first) {
            if(year_open_first > years_low[[1L]])
                return(gettextf("intervals defined by labels \"%s\" and \"%s\" overlap",
                                labels[is_open_first],
                                labels[is_mid][[1L]]))
        }
        if (has_open_last) {
            if(years_up[[n_mid]] > year_open_last)
                return(gettextf("intervals defined by labels \"%s\" and \"%s\" overlap",
                                labels[is_mid][[n_mid]],
                                labels[is_open_last]))
        }
    }
    ## assemble breaks
    breaks <- integer()
    if (has_mid)
        breaks <- c(breaks, years_low, years_up)
    if (has_open_first)
        breaks <- c(breaks, year_open_first)
    if (has_open_last)
        breaks <- c(breaks, year_open_last)
    breaks <- sort(unique(breaks))
    ## return "Label" object
    LabGroupedIntEndpoints(breaks = breaks,
                           open_first = has_open_first,
                           open_last = has_open_last,
                           include_na = has_na)
}

## HAS_TESTS
infer_lab_calendar_quarters <- function(labels) {
    ## regexp patterns
    p_open_first <- "^<([0-9]{4}) Q([1-4])$"
    p_mid <- "^([0-9]{4}) Q([1-4])$"
    p_open_last <- "^([0-9]{4}) Q([1-4])\\+$"
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## sorting should work if labels have correct format
    labels <- sort(labels, na.last = TRUE)
    ## must have at least one non-NA label
    ## (otherwise use different Label class)
    is_na <- is.na(labels)
    if (identical(sum(!is_na), 0L))
        return(gettextf("'%s' has no non-NA elements",
                        "labels"))
    ## initial, non-definitive check that labels have expected format
    is_open_first <- grepl(p_open_first, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_mid <- grepl(p_mid, labels)
    is_invalid <- !(is_open_first | is_mid | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid label for period of one quarter",
                        labels[[i_invalid]]))
    ## summarise
    has_open_first <- any(is_open_first)
    has_open_last <- any(is_open_last)
    has_mid <- any(is_mid)
    has_na <- any(is_na)
    n_mid <- sum(is_mid)
    ## check that at most one 'open_first', 'open_last'
    if (sum(is_open_first) > 1L)
        return(gettextf("two different labels for period open on left : \"%s\" and \"%s\"",
                        labels[is_open_first][[1L]],
                        labels[is_open_first][[2L]]))
    if (sum(is_open_last) > 1L)
        return(gettextf("two different labels for period open on right : \"%s\" and \"%s\"",
                        labels[is_open_last][[1L]],
                        labels[is_open_last][[2L]]))
    ## extract 'dates_mid'
    if (has_mid) {
        years_mid <- sub(p_mid, "\\1", labels[is_mid])
        quarters_mid <- sub(p_mid, "\\2", labels[is_mid])
        months_mid <- (as.integer(quarters_mid) - 1L) * 3L + 1L
        dates_mid <- paste(years_mid, months_mid, 1, sep = "-")
        dates_mid <- as.Date(dates_mid)
    }
    ## extract 'date_open_first', 'date_open_last'
    if (has_open_first) {
        year_open_first <- sub(p_open_first, "\\1", labels[is_open_first])
        quarter_open_first <- sub(p_open_first, "\\2", labels[is_open_first])
        month_open_first <- (as.integer(quarter_open_first) - 1L) * 3L + 1L
        date_open_first <- paste(year_open_first, month_open_first, 1, sep = "-")
        date_open_first <- as.Date(date_open_first)
    }
    if (has_open_last) {
        year_open_last <- sub(p_open_last, "\\1", labels[is_open_last])
        quarter_open_last <- sub(p_open_last, "\\2", labels[is_open_last])
        month_open_last <- (as.integer(quarter_open_last) - 1L) * 3L + 1L
        date_open_last <- paste(year_open_last, month_open_last, 1, sep = "-")
        date_open_last <- as.Date(date_open_last)
    }
    ## extract 'date_mid_min' and 'date_min_max', if present
    if (has_mid) {
        date_mid_min <- min(dates_mid)
        date_mid_max <- max(dates_mid)
    }
    ## make sure 'open_first', 'open_last' do not overlap with each other
    if (has_open_first && has_open_last)
        if (date_open_first > date_open_last)
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_open_first],
                            labels[is_open_last]))
    ## make sure 'open_first', 'open_last' do not overlap with mid
    if (has_mid) {
        if (has_open_first) {
            if (date_open_first > date_mid_min)
                return(gettextf("periods \"%s\" and \"%s\" overlap",
                                labels[is_open_first],
                                labels[is_mid][[1L]]))
        }
        if (has_open_last) {
            if (date_mid_max > date_open_last)
                return(gettextf("periods \"%s\" and \"%s\" overlap",
                                labels[is_mid][[n_mid]],
                                labels[is_open_last]))
        }
    }
    ## get 'break_min' and 'break_max'
    if (has_open_first)
        break_min <- date_open_first
    else if (has_mid)
        break_min <- date_mid_min
    else
        break_min <- date_open_last
    if (has_open_last)
        break_max <- date_open_last
    else if (has_mid)
        break_max <- seq.Date(from = date_mid_max,
                              by = "quarter",
                              length.out = 2L)[[2L]]
    else
        break_max <- date_open_first
    ## return "LabCalendarQuarters" object
    LabCalendarQuarters(break_min = break_min,
                        break_max = break_max,
                        open_first = has_open_first,
                        open_last = has_open_last,
                        include_na = has_na)
}

## HAS_TESTS
infer_lab_calendar_months <- function(labels, gaps_ok = FALSE) {
    ## regexp patterns
    p_open_first <- "^<([0-9]{4}) ([A-z]{3})$"
    p_mid <- "^([0-9]{4}) ([A-z]{3})$"
    p_open_last <- "^([0-9]{4}) ([A-z]{3})\\+$"
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## sorting should work if labels have correct format
    labels <- sort_months(labels)
    ## must have at least one non-NA label
    ## (otherwise use different Label class)
    is_na <- is.na(labels)
    if (identical(sum(!is_na), 0L))
        return(gettextf("'%s' has no non-NA elements",
                        "labels"))
    ## initial, non-definitive check that labels have expected format
    is_open_first <- grepl(p_open_first, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_mid <- grepl(p_mid, labels)
    is_na <- is.na(labels)
    is_invalid <- !(is_open_first | is_mid | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid label for period of one month",
                        labels[[i_invalid]]))
    ## summarise
    has_open_first <- any(is_open_first)
    has_open_last <- any(is_open_last)
    has_mid <- any(is_mid)
    has_na <- any(is_na)
    n_mid <- sum(is_mid)
    ## check that at most one 'open_first', 'open_last'
    if (sum(is_open_first) > 1L)
        return(gettextf("two different labels for period open on left : \"%s\" and \"%s\"",
                        labels[is_open_first][[1L]],
                        labels[is_open_first][[2L]]))
    if (sum(is_open_last) > 1L)
        return(gettextf("two different labels for period open on right : \"%s\" and \"%s\"",
                        labels[is_open_last][[1L]],
                        labels[is_open_last][[2L]]))
    ## extract 'dates_mid', if present
    if (has_mid) {
        years_mid <- sub(p_mid, "\\1", labels[is_mid])
        months_mid <- sub(p_mid, "\\2", labels[is_mid])
        dates_mid <- paste(years_mid, months_mid, 1)
        dates_mid <- as.Date(dates_mid, format = "%Y %b %d")
    }
    ## extract 'date_open_first', 'date_open_last'
    if (has_open_first) {
        year_open_first <- sub(p_open_first, "\\1", labels[is_open_first])
        month_open_first <- sub(p_open_first, "\\2", labels[is_open_first])
        date_open_first <- paste(year_open_first, month_open_first, 1)
        date_open_first <- as.Date(date_open_first, format = "%Y %b %d")
    }
    if (has_open_last) {
        year_open_last <- sub(p_open_last, "\\1", labels[is_open_last])
        month_open_last <- sub(p_open_last, "\\2", labels[is_open_last])
        date_open_last <- paste(year_open_last, month_open_last, 1)
        date_open_last <- as.Date(date_open_last, format = "%Y %b %d")
    }
    ## extract 'date_mid_min' and 'date_min_max', if present
    if (has_mid) {
        date_mid_min <- min(dates_mid)
        date_mid_max <- max(dates_mid)
    }
    ## make sure 'open_first', 'open_last' do not overlap with each other
    if (has_open_first && has_open_last)
        if (date_open_first > date_open_last)
            return(gettextf("periods \"%s\" and \"%s\" overlap",
                            labels[is_open_first],
                            labels[is_open_last]))
    ## make sure 'open_first', 'open_last' do not overlap with each other
    if (has_mid) {
        if (has_open_first) {
            if (date_open_first > date_mid_min)
                return(gettextf("periods \"%s\" and \"%s\" overlap",
                                labels[is_open_first],
                                labels[is_mid][[1L]]))
        }
        if (has_open_last) {
            if (date_mid_max > date_open_last)
                return(gettextf("periods \"%s\" and \"%s\" overlap",
                                labels[is_mid][[n_mid]],
                                labels[is_open_last]))
        }
    }
    ## get 'break_min' and 'break_max'
    if (has_open_first)
        break_min <- date_open_first
    else if (has_mid)
        break_min <- date_mid_min
    else
        break_min <- date_open_last
    if (has_open_last)
        break_max <- date_open_last
    else if (has_mid)
        break_max <- seq.Date(from = date_mid_max,
                              by = "month",
                              length.out = 2L)[[2L]]
    else
        break_max <- date_open_first
    ## return "LabCalendarMonth" object
    LabCalendarMonths(break_min = break_min,
                      break_max = break_max,
                      open_first = has_open_first,
                      open_last = has_open_last,
                      include_na = has_na)
}




infer_lab_durations_quarters <- function(labels) {
    ## regexp patterns
    p_mid <- "^([0-9]+)q$"
    p_open_last <- "^([0-9]+)q\\+$"
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels,
                                      name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## sorting should work if labels have correct format
    labels <- sort_durations(labels)
    ## must have at least one non-NA label
    ## (otherwise use different Label class)
    is_na <- is.na(labels)
    if (identical(sum(!is_na), 0L))
        return(gettextf("'%s' has no non-NA elements",
                        "labels"))
    ## initial, non-definitive check that labels have expected format
    is_mid <- grepl(p_mid, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_invalid <- !(is_mid | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid label for interval of one quarter",
                        labels[[i_invalid]]))
    ## summarise
    has_mid <- any(is_mid)
    has_open_last <- any(is_open_last)
    has_na <- any(is_na)
    n_mid <- sum(is_mid)
    ## check that at most one  'open_last'
    if (sum(is_open_last) > 1L)
        return(gettextf("two different labels for interval open on right : \"%s\" and \"%s\"",
                        labels[is_open_last][[1L]],
                        labels[is_open_last][[2L]]))
    ## extract 'quarters_mid', if present
    if (has_mid) {
        quarters_mid <- sub(p_mid, "\\1", labels[is_mid])
        quarters_mid <- as.integer(quarters_mid)
        quarters_mid_min <- quarters_mid[[1L]]
        quarters_mid_max <- quarters_mid[[n_mid]]
    }
    ## extract 'quarter_open_last'
    if (has_open_last) {
        quarter_open_last <- sub(p_open_last, "\\1", labels[is_open_last])
        quarter_open_last <- as.integer(quarter_open_last)
    }
    ## make sure 'open_last' does not overlap with mid   
    if (has_mid && has_open_last) {
        if (quarters_mid_max >= quarter_open_last)
            return(gettextf("intervals \"%s\" and \"%s\" overlap",
                            labels[is_mid][[n_mid]],
                            labels[is_open_last]))
    }
    ## get 'break_min' and 'break_max'
    if (has_mid)
        break_min <- quarters_mid_min
    else
        break_min <- quarter_open_last
    if (has_open_last)
        break_max <- quarter_open_last
    else
        break_max <- quarters_mid_max + 1L
    ## return "LabDurationsQuarters" object
    LabDurationsQuarters(break_min = break_min,
                         break_max = break_max,
                         open_last = has_open_last,
                         include_na = has_na)
}

## HAS_TESTS
infer_lab_durations_months <- function(labels) {
    ## regexp patterns
    p_mid <- "^([0-9]+)m$"
    p_open_last <- "^([0-9]+)m\\+$"
    ## check for blanks
    demcheck::err_is_not_blank_vector(x = labels, name = "labels")
    ## only need to process one instance of each label
    labels <- unique(labels)
    ## sorting should work if labels have correct format
    labels <- sort_durations(labels)
    ## must have at least one non-NA label
    ## (otherwise use different Label class)
    is_na <- is.na(labels)
    if (identical(sum(!is_na), 0L))
        return(gettextf("'%s' has no non-NA elements",
                        "labels"))
    ## initial, non-definitive check that labels have expected format
    is_mid <- grepl(p_mid, labels)
    is_open_last <- grepl(p_open_last, labels)
    is_invalid <- !(is_mid | is_open_last | is_na)
    i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
    if (i_invalid > 0L)
        return(gettextf("\"%s\" not a valid label for interval of one month",
                        labels[[i_invalid]]))
    ## summarise
    has_mid <- any(is_mid)
    has_open_last <- any(is_open_last)
    has_na <- any(is_na)
    n_mid <- sum(is_mid)
    ## check that at most one 'open_last'
    if (sum(is_open_last) > 1L)
        return(gettextf("two different labels for interval open on right : \"%s\" and \"%s\"",
                        labels[is_open_last][[1L]],
                        labels[is_open_last][[2L]]))
    ## extract 'months_mid', if present
    if (has_mid) {
        months_mid <- sub(p_mid, "\\1", labels[is_mid])
        months_mid <- as.integer(months_mid)
        months_mid_min <- months_mid[[1L]]
        months_mid_max <- months_mid[[n_mid]]
    }
    ## extract 'month_open_last'
    if (has_open_last) {
        month_open_last <- sub(p_open_last, "\\1", labels[is_open_last])
        month_open_last <- as.integer(month_open_last)
    }
    ## make sure 'open_last' does not overlap with mid   
    if (has_mid && has_open_last) {
        if (months_mid_max >= month_open_last)
            return(gettextf("intervals \"%s\" and \"%s\" overlap",
                            labels[is_mid][[n_mid]],
                            labels[is_open_last]))
    }
    ## get 'break_min' and 'break_max'
    if (has_mid)
        break_min <- months_mid_min
    else
        break_min <- month_open_last
    if (has_open_last)
        break_max <- month_open_last
    else
        break_max <- months_mid_max + 1L
    ## return "LabDurationsMonths" object
    LabDurationsMonths(break_min = break_min,
                       break_max = break_max,
                       open_last = has_open_last,
                       include_na = has_na)
}
