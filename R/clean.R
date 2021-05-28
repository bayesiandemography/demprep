
#' Tidy age group labels
#'
#' Try to parse age group
#' labels and convert them
#' to the format used by the dem packages.
#'
#' Intervals that are open on the right
#' such as \code{"80+"} are
#' allowed. Intervals that are open on the left
#' such as \code{"<20"} are not.
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
#' \code{clean_age} does not remove month or quarter
#' labels, as this could result in ambiguity when
#' different age groups use different units.
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
#' x <- c("100 and over", ## open on right
#'        "<10",          ## open on left
#'        "infants",
#'        "10 to 19 years",
#'        "infants",
#'        "untranslatable",
#'        "10-19",
#'        "100 quarters",
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


#' Tidy cohort labels
#'
#' Try to parse cohort
#' labels and convert them
#' to the format used by the dem packages.
#'
#' Intervals that are open on the left
#' such as \code{"<2000"} are
#' allowed. Intervals that are open on the right
#' such as \code{"2000+"} are not.
#'
#' By default, \code{clean_cohort} assumes that any
#' text labels are written in English. However,
#' other languages can be specified using
#' the \code{language} argument. Current choices are
#' ADD OVER TIME.
#'
#' Function \code{clean_cohort_df} returns a data frame
#' showing how each unique element in \code{x} is
#' interpreted by function \code{clean_cohort} and whether
#' the element can be interpreted as a valid
#' cohort label. \code{clean_cohort_df} can be
#' used to check whether \code{clean_cohort} is
#' giving the desired results.
#'
#' @inheritParams clean_age
#'
#' @return
#' \code{clean_cohort} returns a character vector with the same
#' length as \code{x} in which labels that have been
#' parsed are translated to dem formats.
#' \code{clean_cohort_df} returns a data frame with columns
#' \code{"input"}, \code{"output"}, and \code{"is_valid"}.
#'
#' @examples
#' x <- c("before 2000",  ## open on left
#'        "after 2000",   ## open on right
#'        "Millenials",
#'        "2020 Jan",
#'        "Q3 2020",
#'        "January 2020",
#'        "2025 first quarter",
#'        "untranslatable",
#'        "2020-2025")
#' x
#' clean_cohort(x)
#' clean_cohort_df(x)
#' @name clean_cohort
NULL

## HAS_TESTS
#' @export
#' @rdname clean_cohort
clean_cohort <- function(x, language = "English") {
    language <- match.arg(language)
    ans <- x
    x_guess <- clean_cohort_period_guess(x = x,
                                         language = language,
                                         open_first = TRUE)
    is_valid <- is_valid_cohort(x_guess)
    ans[is_valid] <- x_guess[is_valid]
    ans
}

## HAS_TESTS
#' @export
#' @rdname clean_cohort
clean_cohort_df <- function(x, language = "English") {
    x_processed <- clean_cohort(x = x,
                                language = language)
    is_duplicated <- duplicated(x)
    input <- x[!is_duplicated]
    output <- x_processed[!is_duplicated]
    is_valid <- is_valid_cohort(output)
    data.frame(input = input,
               output = output,
               is_valid = is_valid)
}


#' Tidy period labels
#'
#' Try to parse period
#' labels and convert them
#' to the format used by the dem packages.
#'
#' Open intervals such as \code{"<2020"}
#' or \code{"2020+"} are not allowed.
#'
#' By default, \code{clean_period} assumes that any
#' text labels are written in English. However,
#' other languages can be specified using
#' the \code{language} argument. Current choices are
#' ADD OVER TIME.
#'
#' Function \code{clean_period_df} returns a data frame
#' showing how each unique element in \code{x} is
#' interpreted by function \code{clean_period} and whether
#' the element can be interpreted as a valid
#' period label. \code{clean_period_df} can be
#' used to check whether \code{clean_period} is
#' giving the desired results.
#'
#' @inheritParams clean_age
#'
#' @return
#' \code{clean_period} returns a character vector with the same
#' length as \code{x} in which labels that have been
#' parsed are translated to dem formats.
#' \code{clean_period_df} returns a data frame with columns
#' \code{"input"}, \code{"output"}, and \code{"is_valid"}.
#'
#' @examples
#' x <- c("before 2000", ## open on left
#'        "after 2000",  ## open on right
#'        "2020 Jan",
#'        "Q3 2020",
#'        "January 2020",
#'        "2025 first quarter",
#'        "untranslatable",
#'        "2020-2025")
#' x
#' clean_period(x)
#' clean_period_df(x)
#' @name clean_period
NULL

## HAS_TESTS
#' @export
#' @rdname clean_period
clean_period <- function(x, language = "English") {
    language <- match.arg(language)
    ans <- x
    x_guess <- clean_cohort_period_guess(x = x,
                                         language = language,
                                         open_first = FALSE)
    is_valid <- is_valid_period(x_guess)
    ans[is_valid] <- x_guess[is_valid]
    ans
}

## HAS_TESTS
#' @export
#' @rdname clean_period
clean_period_df <- function(x, language = "English") {
    x_processed <- clean_period(x = x,
                                language = language)
    is_duplicated <- duplicated(x)
    input <- x[!is_duplicated]
    output <- x_processed[!is_duplicated]
    is_valid <- is_valid_period(output)
    data.frame(input = input,
               output = output,
               is_valid = is_valid)
}
