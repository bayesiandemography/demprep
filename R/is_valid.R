
## NO_TESTS
#' Check for valid age group labels
#'
#' Identify the elements of a vector that are
#' valid labels for age groups. Each element is assessed
#' independently, so the labels are not necessarily
#' consistent with each other.
#'
#' @param x A vector.
#'
#' @return A logical vector, the same length as \code{x}.
#'
#' @seealso \code{\link{make_labels_age}} describes the rules
#' governing labels for age groups.
#'
#' @examples
#' ## years
#' x <- c("1-4", "12-10", "100+", "old")
#' is_valid_age(x)
#'
#' ## quarters
#' x <- c("44q", "100q+")
#' is_valid_age(x)
#'
#' ## months
#' x <- c("3m", "12m+")
#' is_valid_age(x)
#'
#' ## NAs are not valid age groups
#' x <- c("10-14", NA)
#' is_valid_age(x)
#'
#' ## age groups are allowed to overlap,
#' ## and to mix years, quarters, and months,
#' ## provided that each individual
#' ## label is valid
#' x <- c("10-19", "15-19", "3q", "0m")
#' is_valid_age(x)
#' @export
is_valid_age <- function(x) {
    ## define patterns
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    p_single <- "^[0-9]+[qm]?$"
    p_open_last <- "^[0-9]+[qm]?\\+$"
    p_other <- paste(p_single, p_open_last, sep = "|")
    ## check for format "<low>-<up>", including
    ## verifying that 'up' is greater than 'low'
    is_low_up <- grepl(p_low_up, x)
    low <- sub(p_low_up, "\\1", x[is_low_up])
    up <- sub(p_low_up, "\\2", x[is_low_up])
    low <- as.integer(low)
    up <- as.integer(up)
    is_valid_low_up <- up > low
    is_low_up[is_low_up] <- is_valid_low_up
    ## check for remaining possibilities
    is_other <- grepl(p_other, x)
    ## return TRUE if consistent with any pattern
    is_low_up | is_other
}

