
## HAS_TESTS
#' Check for valid age group labels
#'
#' Identify the elements of a vector that are
#' valid labels for age groups. Each element is assessed
#' independently, so the labels can overlap.
#'
#' \code{NA}s are treated as valid labels.
#'
#' @param x A vector.
#'
#' @return A logical vector, the same length as \code{x}.
#'
#' @examples
#' ## years
#' x <- c("1-4", "12-10", "100+", "old", NA)
#' is_valid_age(x)
#'
#' ## age groups are allowed to overlap,
#' ## provided that each individual
#' ## label is valid
#' x <- c("10-19", "15-19", "5+")
#' is_valid_age(x)
#' @export
is_valid_age <- function(x) {
    ## check for format "<low>-<up>", including
    ## verifying that 'up' is greater than 'low'
    is_low_up <- grepl(CONST_P_LOW_UP, x)
    low <- sub(CONST_P_LOW_UP, "\\1", x[is_low_up])
    up <- sub(CONST_P_LOW_UP, "\\2", x[is_low_up])
    low <- as.integer(low)
    up <- as.integer(up)
    is_valid_low_up <- up > low
    is_low_up[is_low_up] <- is_valid_low_up
    ## check for remaining possibilities
    is_single <- grepl(CONST_P_SINGLE, x)
    is_open_last <- grepl(CONST_P_OPEN_LAST, x)
    is_na <- is.na(x)
    ## return TRUE if consistent with any pattern or is NA
    is_low_up | is_single | is_open_last | is_na
}


## HAS_TESTS
#' Check for valid cohort labels
#'
#' Identify the elements of a vector that are
#' valid labels for cohorts. Each element is assessed
#' independently, so \code{x} can contain
#' a mix of different single-year, multiple-year,
#' quarter, and month formats, and labels
#' can overlap.
#'
#' \code{NA}s are treated as valid labels.
#'
#' @param x A vector.
#'
#' @return A logical vector, the same length as \code{x}.
#'
#' @examples
#' ## years
#' x <- c("2030-2035", "2021", "<2000", "Gen Z", NA,
#'        "2020 Q4", "2021 Sep", "<2022 Jul")
#' is_valid_cohort(x)
#' @export
is_valid_cohort <- function(x) {
    ## check for format "<low>-<up>", including
    ## verifying that 'up' is greater than 'low'
    is_low_up <- grepl(CONST_P_LOW_UP, x)
    low <- sub(CONST_P_LOW_UP, "\\1", x[is_low_up])
    up <- sub(CONST_P_LOW_UP, "\\2", x[is_low_up])
    low <- as.integer(low)
    up <- as.integer(up)
    is_valid_low_up <- up > low
    is_low_up[is_low_up] <- is_valid_low_up
    ## check for remaining possibilities
    is_single <- grepl(CONST_P_SINGLE, x)
    is_open_first <- grepl(CONST_P_OPEN_FIRST, x)
    is_single_quarter <- grepl(CONST_P_SINGLE_QUARTER, x)
    is_open_first_quarter <- grepl(CONST_P_OPEN_FIRST_QUARTER, x)
    is_single_month <- grepl(CONST_P_SINGLE_MONTH, x)
    is_open_first_month <- grepl(CONST_P_OPEN_FIRST_MONTH, x)
    is_na <- is.na(x)
    ## return TRUE if consistent with any pattern or is NA
    (is_low_up
        | is_single
        | is_open_first
        | is_single_quarter
        | is_open_first_quarter
        | is_single_month
        | is_open_first_month
        | is_na)
}


## HAS_TESTS
#' Check for valid period labels
#'
#' Identify the elements of a vector that are
#' valid labels for periods. Each element is assessed
#' independently, so \code{x} can contain
#' a mix of different single-year, multiple-year,
#' quarter, and month formats, and labels
#' can overlap.
#'
#' \code{NA}s are treated as valid labels.
#'
#' @param x A vector.
#'
#' @return A logical vector, the same length as \code{x}.
#'
#' @examples
#' ## years
#' x <- c("2030-2035", "2021", "2000", "Now", NA,
#'        "2020 Q4", "2021 Sep", "<2022 Jul")
#' is_valid_period(x)
#' @export
is_valid_period <- function(x) {
    ## check for format "<low>-<up>", including
    ## verifying that 'up' is greater than 'low'
    is_low_up <- grepl(CONST_P_LOW_UP, x)
    low <- sub(CONST_P_LOW_UP, "\\1", x[is_low_up])
    up <- sub(CONST_P_LOW_UP, "\\2", x[is_low_up])
    low <- as.integer(low)
    up <- as.integer(up)
    is_valid_low_up <- up > low
    is_low_up[is_low_up] <- is_valid_low_up
    ## check for remaining possibilities
    is_single <- grepl(CONST_P_SINGLE, x)
    is_single_quarter <- grepl(CONST_P_SINGLE_QUARTER, x)
    is_single_month <- grepl(CONST_P_SINGLE_MONTH, x)
    is_na <- is.na(x)
    ## return TRUE if consistent with any pattern or is NA
    (is_low_up
        | is_single
        | is_single_quarter
        | is_single_month
        | is_na)
}
