
## HAS_TESTS
#' Convert a vector of month labels to list of pairs of dates
#'
#' Given a vector of labels for months
#' return a list of dates defining those months.
#' Assume that if \code{x} includes an open interval
#' that open interval is allowed to be there.
#'
#' The items in the list all have length two. If an item represents
#' a closed interval, then the first element gives the date of the first
#' day of the interval, and the second element gives the date of
#' the first day of the next interval. If the item represents an
#' interval that is open on the left, then the first element
#' is \code{as.Date(NA)} and the second element is the first
#' day of the next interval. If the item represents a missing value,
#' then the first element and second element are both \code{as.Date(NA)}.
#'
#' The elements are not assumed to be ordered, and there may be
#' more than one open interval and more than one missing value.
#' 
#' @param x A vector of labels.
#' @param name The name for \code{x} to be used in error messages
#'
#' @return A list, the items of which are Date
#' vectors of length 2.
#'
#' @seealso \code{\link{labels_to_dates_year}},
#' \code{\link{labels_to_dates_quarter}}
#'
#' @examples
#' x <- c("<2019 Aug", NA, "2011 Feb", "2012 Mar", NA)
#' labels_to_dates_month(x, name = "x")
#' @keywords internal
#' @export
labels_to_dates_month <- function(x, name) {
    ## regular expressions
    p_single <- paste(sprintf("^[0-9]+ %s$", month.abb), collapse = "|")
    p_open <- paste(sprintf("^<[0-9]+ %s$", month.abb), collapse = "|")
    ## check arguments
    demcheck::err_is_string(x = name,
                            name = "name")
    ## classify labels, rasing error for invalid ones
    is_na <- is.na(x)
    is_single <- grepl(p_single, x)
    is_open <- grepl(p_open, x)
    is_valid <- is_na | is_single | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" in '%s' is not a valid label",
                      x[[i_invalid]], name),
             call. = FALSE)
    ## extract lower and upper dates
    n <- length(x)
    date_low <- rep(as.Date(NA), times = n)
    date_high <- date_low
    date_low[is_single] <- date_start_month(x[is_single])
    date_high[is_single] <- add_months(date_low[is_single],
                                       n = 1L)
    date_high[is_open] <- date_start_month(x[is_open])
    ## format as list and return
    ans <- mapply(c, date_low, date_high, SIMPLIFY = FALSE)
    ans
}


## HAS_TESTS
#' Convert a vector of quarter labels to list of pairs of dates
#'
#' Given a vector of labels for quarters (ie three month intervals),
#' return a list of dates defining those quarters.
#' Assume that if \code{x} includes an open interval
#' that open interval is allowed to be there.
#'
#' The items in the list all have length two. If an item represents
#' a closed interval, then the first element gives the date of the first
#' day of the interval, and the second element gives the date of
#' the first day of the next interval. If the item represents an
#' interval that is open on the left, then the first element
#' is \code{as.Date(NA)} and the second element is the first
#' day of the next interval. If the item represents a missing value,
#' then the first element and second element are both \code{as.Date(NA)}.
#'
#' The elements are not assumed to be ordered, and there may be
#' more than one open interval and more than one missing value.
#' 
#' @param x A vector of labels.
#' @param name The name for \code{x} to be used in error messages
#'
#' @return A list, the items of which are Date
#' vectors of length 2.
#'
#' @seealso \code{\link{labels_to_dates_year}},
#' \code{\link{labels_to_dates_month}}
#'
#' @examples
#' x <- c("<2019 Q3", NA, "2011 Q1", "2012 Q1", NA)
#' labels_to_dates_quarter(x, name = "x")
#' @keywords internal
#' @export
labels_to_dates_quarter <- function(x, name) {
    ## regular expressions
    p_single <- "^[0-9]+ Q[1-4]$"
    p_open <- "^<[0-9]+ Q[1-4]$"
    ## check arguments
    demcheck::err_is_string(x = name,
                            name = "name")
    ## classify labels, rasing error for invalid ones
    is_na <- is.na(x)
    is_single <- grepl(p_single, x)
    is_open <- grepl(p_open, x)
    is_valid <- is_na | is_single | is_open
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L)
        stop(gettextf("\"%s\" in '%s' is not a valid label",
                      x[[i_invalid]], name),
             call. = FALSE)
    ## extract lower and upper dates
    n <- length(x)
    date_low <- rep(as.Date(NA), times = n)
    date_high <- date_low
    date_low[is_single] <- date_start_quarter(x[is_single])
    date_high[is_single] <- add_quarters(date_low[is_single],
                                         n = 1L)
    date_high[is_open] <- date_start_quarter(x[is_open])
    ## format as list and return
    ans <- mapply(c, date_low, date_high, SIMPLIFY = FALSE)
    ans
}
