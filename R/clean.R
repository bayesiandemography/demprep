
#' Tidy age group labels
#'
#' Try to parse age group
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
#' By default, \code{clean_age} assumes that any
#' text labels are written in English. However,
#' other languages can be specified using
#' the \code{language} argument. Current choices are
#' ADD OVER TIME.
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
#' @param x A numeric or character vector.
#' @param language The language in which text
#' labels are written. Defaults to English.
#'
#' @return
#' \code{clean_age} returns a character vector with the same
#' length as \code{x} in which labels that have been
#' parsed are translated to dem formats.
#' \code{clean_age_df} returns a data frame with columns
#' \code{"input"}, \code{"output"}, and \code{"is_valid"}.
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
clean_age <- function(x, language = "English") {
    language <- match.arg(language)
    ans <- x
    x_guess <- clean_age_guess(x = x,
                               language = language)
    is_valid <- is_valid_age(x_guess)
    ans[is_valid] <- x_guess[is_valid]
    ans
}

## HAS_TESTS
#' @export
#' @rdname clean_age
clean_age_df <- function(x, language = "English") {
    x_processed <- clean_age(x = x,
                             language = language)
    is_duplicated <- duplicated(x)
    input <- x[!is_duplicated]
    output <- x_processed[!is_duplicated]
    is_valid <- is_valid_age(output)
    data.frame(input = input,
               output = output,
               is_valid = is_valid)
}
