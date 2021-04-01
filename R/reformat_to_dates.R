
## reformat_to_dates_year <- function(x,
##                                    month_start = "Jan",
##                                    label_year_start = TRUE) {
##     ## regexp patterns
##     p_single <- "^-?[0-9]+$"
##     p_open <- "^<-?[0-9]+$"
##     ## check arguments
##     demcheck::err_is_factor(x = x,
##                             name = "x")
##     levels_old <- levels(x)
##     levels_old_processed <- format_cohort_year(x = levels)
##     if (!identical(levels_old_processed, levels_old))
##         stop(gettextf("levels for '%s' do not have format expected for one-year periods or cohorts",
##                       "x"),
##              call. = FALSE)
##     month_start <- demcheck::err_tdy_month_start(x = month_start,
##                                                  name = "month_start")
##     demcheck::err_is_logical_flag(x = label_year_start,
##                                   name = "label_year_start")
##     ## classify levels_old
##     is_na <- is.na(labels_old)
##     is_single <- grepl(p_single, labels_old)
##     is_open <- grepl(p_open, labels_old)
##     ## extract lower and upper years
##     year_low <- rep(NA_integer_, times = length(levels_old))
##     year_up <- year_low
##     years_single <- as.integer(labels_old[is_single])
##     year_low[is_single] <- years_single + label_year_start
##     year_up[is_single] <- years_single + 1L + label_year_start
##     year_up[is_open] <- as.integer(sub("^<", "", labels_old[is_open]))
##     ## convert to dates
##     date_low <- paste(year_low, month_start, "1")
##     date_low[is_na] <- NA_character_
##     date_low <- as.Date(year_low, format = "%Y %b %d")
##     date_up <- paste(year_up, month_start, "1")
##     date_up <- as.Date(year_up, format = "%Y %b %d")
##     ## make new levels
##     levels_new <- make_labels_dates(date_low = date_low,
##                                     date_up = date_up,
##                                     is_open = is_open,
##                                     is_na = is_na)
##     ## return factor with new levels
##     ans <- x[levels_new]
##     ans <- factor(ans,
##                   levels = levels_new,
##                   exclude = NULL)
##     ans
## }


## reformat_to_dates_multi <- function(x,
##                                     month_start = "Jan") {
##     stop("not written yet")
## }


## reformat_to_dates_custom <- function(x,
##                                     month_start = "Jan") {
##     stop("not written yet")
## }


## reformat_to_dates_quarter <- function(x) {
##     stop("not written yet")
## }


## reformat_to_dates_month <- function(x) {
##     stop("not written yet")
## }




