
## Functions to help with imputation that end users do not use directly.

## HAS_TESTS
impute_date_day <- function(year, month) {
    origin <- as.Date("1970-01-01")
    days_origin <- as.integer(origin)
    days_in_month_tab <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    n <- length(year)
    days_in_month <- days_in_month_tab[month]
    is_leap_year <- !is.na(year) & is_leap_year(year)
    is_feb <- !is.na(month) & (month == 2L)
    days_in_month[is_leap_year & is_feb] <- 29L
    date_1_month <- ifelse(is.na(year) | is.na(month),
                           NA_character_,
                           sprintf("%s-%s-01",
                                   year,
                                   month))
    date_1_month <- as.Date(date_1_month)
    days_from_origin_1_month <- as.integer(date_1_month) - days_origin
    offset <- as.integer(stats::runif(n) * days_in_month)
    at_upper_limit <- !is.na(offset) & (offset == days_in_month)  ## unlikely, but check anyway
    offset[at_upper_limit] <- offset[at_upper_limit] - 1L
    days_ans <- days_from_origin_1_month + offset
    ans <- as.Date(days_ans, origin = origin)
    ans
}

## HAS_TESTS
impute_date_month <- function(year, day) {
    ## months orderd by number of days in non-leap year:
    month_tab <- c(1L, 3L, 5L, 7L, 8L, 10L, 12L, 4L, 6L, 9L, 11L, 2L)
    n <- length(year)
    is_leap_year <- !is.na(year) & is_leap_year(year)
    is_day_gt_29 <- !is.na(day) & (day > 29L)
    is_non_leap_29 <- !is.na(year) & !is.na(day) & !is_leap_year & (day == 29L)
    not_feb <- is_day_gt_29 | is_non_leap_29
    not_feb_apr_jun_sep_nov <- !is.na(day) & (day == 31L)
    i_month_max <- ifelse(is.na(year) | is.na(day),
                          NA_integer_,
                          12L)
    i_month_max[not_feb] <- 11L
    i_month_max[not_feb_apr_jun_sep_nov] <- 7L
    i_month <- as.integer(stats::runif(n) * i_month_max) + 1L
    over_limit <- !is.na(i_month) & (i_month > i_month_max) ## unlikely, but check anyway
    i_month[over_limit] <- i_month_max[over_limit]
    month <- month_tab[i_month]
    ans <- ifelse(is.na(year) | is.na(month) | is.na(day),
                  NA_character_,
                  sprintf("%s-%s-%s",
                          year,
                          month,
                          day))
    ans <- as.Date(ans)
    ans
}

## HAS_TESTS
impute_date_month_day <- function(year) {
    origin <- as.Date("1970-01-01")
    days_origin <- as.integer(origin)
    n <- length(year)
    is_leap_year <- !is.na(year) & is_leap_year(year)
    days_in_year <- ifelse(is.na(year),
                           NA_integer_,
                    ifelse(is_leap_year,
                           366L,
                           365L))
    date_1_jan <- ifelse(is.na(year),
                         NA_character_,
                         sprintf("%s-01-01", year))
    date_1_jan <- as.Date(date_1_jan)
    days_from_origin_1_jan <- as.integer(date_1_jan) - days_origin
    offset <- as.integer(stats::runif(n) * days_in_year)
    at_upper_limit <- !is.na(offset) & (offset == days_in_year)  ## unlikely, but check anyway
    offset[at_upper_limit] <- offset[at_upper_limit] - 1L
    days_ans <- days_from_origin_1_jan + offset
    ans <- as.Date(days_ans, origin = origin)
    ans
}        


    
    
    
    
    
 
