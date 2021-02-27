
#' Reformat age group labels
#'
#' Try to parse English-language age group
#' labels and convert them
#' to the format used by the dem packages.
#'
#' The basic strategy followed by \code{clean_age}
#' is to strip off redundant words such as "years",
#' and to translate to formats
#' used by the dem packages, such as translating
#' \code{"and over"} to \code{"+"} or "months"
#' to "m".
#'
#' \code{clean_age} also checks for two special
#' cases: (i) when the labels consist entirely of numbers
#' \code{0}, \code{5}, \code{10}, \dots,
#' \code{A}, and (ii) when the labels consist entirely
#' of the numbers \code{0}, \code{1}, \code{5},
#' \code{10}, \dots, \code{A}. In case
#' (i) the labels are converted to the age groups
#' \code{"0-4"}, \code{"5-9"}, \code{"10-14"},
#' \dots, \code{"A+"}. In case (ii)
#' the labels are converted to the life table age groups
#' \code{"0"}, \code{"1-4"}, \code{"5-9"},
#' \dots, \code{"A+"}. In both cases, \code{A}
#' must be at least 50. 
#'
#' Function \code{clean_age_df} returns a data frame
#' showing how each unique element in \code{x} is
#' interpreted by function \code{clean_age} and whether
#' the element can be interpreted as a valid
#' age group label. \code{clean_age_df} can be
#' used to check whether \code{clean_age} is
#' giving the desired results.
#'
#' @section Warning:
#' \code{clean_age} is designed for interactive use and
#' one-off analyses, where a human scrutinises the results.
#' \code{clean_age} may not be appropriate for
#' automated production processes where inputs
#' can change over time. 
#' 
#' @param x A numeric or character vector.
#'
#' @return
#' \code{clean_age} returns a character vector with the same
#' length as \code{x} in which labels that have been
#' parsed are translated to dem formats.
#' \code{clean_age_df} returns a data frame with columns
#' \code{"input"}, \code{"output"}, and \code{"is_valid"}.
#'
#' @seealso \code{\link{make_labels_age}} describes the
#' rules for formating age group labels in the dem packages.
#'
#' @examples
#' x <- c("100 and over",
#'        "infants",
#'        "10 to 19 years",
#'        "infants",
#'        "untranslatable",
#'        "10-19",
#'        "100 quarters or more",
#'        "also untranslatable",
#'        "three months")
#' x
#' clean_age(x)
#' clean_age_df(x)
#'
#' ## 5-year age groups defined by starting age
#' x <- sample(seq(0, 80, 5))
#' x
#' clean_age(x)
#' clean_age_df(x)
#'
#' ## age groups commonly used by life tables
#' x <- sample(c(0, 1, seq(5, 80, 5)))
#' x
#' clean_age(x)
#' clean_age_df(x)
#' @name clean_age
NULL

## HAS_TESTS
#' @export
#' @rdname clean_age
clean_age <- function(x) {
    ans <- x
    x_guess <- clean_age_guess(x)
    is_valid <- is_valid_age(x_guess)
    ans[is_valid] <- x_guess[is_valid]
    ans
}

## HAS_TESTS
#' @export
#' @rdname clean_age
clean_age_df <- function(x) {
    x_processed <- clean_age(x)
    is_duplicated <- duplicated(x)
    input <- x[!is_duplicated]
    output <- x_processed[!is_duplicated]
    is_valid <- is_valid_age(output)
    data.frame(input = input,
               output = output,
               is_valid = is_valid)
}


## ## need to allow for medians
## cleanquantile <- function(x) {
##     x_is_na <- is.na(x)
##     if (all(x_is_na))
##         return(x)
##     is_neg <- grepl("-[0-9]", x)
##     i_neg <- match(TRUE, is_neg, nomatch = 0L)
##     has_neg <- i_neg > 0L
##     if (has_neg) {
##         stop(gettextf("element %d of '%x' [\"%s\"] appears to contain negative number",
##                       i_neg, "x", x[[i_neg]]),
##              call. = FALSE)
##     }
##     x_no_text <- gsub("[^0-9.]", "", x)
##     x_num <- as.numeric(x_no_text)
##     is_invalid <- !x_is_na & is.na(x_num)
##     i_invalid <- match(TRUE, is_invalid, nomatch = 0L)
##     has_invalid <- i_invalid > 0L
##     if (has_invalid) {
##         stop(gettextf("don't know how to interpret element %d of '%x' [\"%s\"] as quantile",
##                       i_invalid, "x", x[[i_invalid]]),
##              call. = FALSE)
##     }
##     x_num_non_na <- x_num[!x_is_na]
##     all_le_1 <- all(x_num_na <= 1L)
##     if (all_le_1)
##         return(paste0(100 * x_num, "%"))
##     all_le_100 <- all(x_num_na <= 100L)
##     if (all_le_100)
##         return(paste0(x_num, "%"))
##     stop(gettextf(paste("don't know how to interpret '%s' as quantiles because",
##                         "not all values between 0 and 1 or between 0 and 100"),
##                   "x"))
## }
