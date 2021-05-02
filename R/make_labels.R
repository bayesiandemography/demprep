
## HAS_TESTS
#' Make Categories labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Categories"}
#' (as defined in the demarray package).
#' This functions would not normally be called directly
#' by end users.
#'
#' \code{x} is a character vector, with no duplicates or blanks
#' (ie strings with zero characters, \code{""}).
#' 
#' @param x A character vector.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#'
#' @examples
#' make_labels_categories(x = c("Nigeria",
#'                              "Ethiopia",
#'                              NA,
#'                              "South Africa"))
#' @keywords internal
#' @export
make_labels_categories <- function(x) {
    demcheck::err_values_categories(x = x,
                                    name = "x")
    x
}

## HAS_TESTS
#' Make Triangles labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Triangles"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a character vector, with no duplicates.
#' The elements of \code{x} for must belong
#' to \code{c("Lower", "Upper", NA)}.
#' 
#' @param x A character vector.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#'
#' @examples
#' make_labels_triangles(x = c("Upper", NA, "Lower"))
#' @keywords internal
#' @export
make_labels_triangles <- function(x) {
    demcheck::chk_values_triangles(x = x,
                                   name = "x")
    x
}

## HAS_TESTS
#' Make Directions labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Directions"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a character vector, with no duplicates.
#' The elements of \code{x} must belong
#' to \code{c("In", "Out" NA)}.
#' 
#' @param x A character vector.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#'
#' @examples
#' make_labels_directions(x = c("In", NA, "Out"))
#' @keywords internal
#' @export
make_labels_directions <- function(x) {
    demcheck::err_values_directions(x = x,
                                    name = "x")
    x
}

## HAS_TESTS
#' Make Quantiles labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Quantiles"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a character vector, with no duplicates.
#' The elements of \code{x} must be
#' a number between 0 and 100 (inclusive),
#' followed by a "\%" sign,
#' or be an \code{NA}.
#' 
#' @param x A character vector.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#'
#' @examples
#' make_labels_quantiles(x = c("97.5%", NA, "2.5%", "50%"))
#' @keywords internal
#' @export
make_labels_quantiles <- function(x) {
    demcheck::err_values_quantiles(x = x,
                                   name = "x")
    x
}


## HAS_TESTS
#' Make Integers labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Integers"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of integer vectors of length 2.
#' If elements of a vector are non-NA, then the second
#' element must be one greater than the first.
#' The vectors cannot overlap.
#'
#' The elements of \code{x} are converted into
#' labels as follows:
#'
#' \tabular{ll}{
#'   Element \tab Label \cr
#'   \code{c(100L, 101L)} \tab \code{"100"} \cr
#'   \code{c(NA_integer_, 100L)} \tab \code{"<100"} \cr
#'   \code{c(100L, NA_integer_)} \tab \code{"100+"} \cr
#'   \code{c(NA_integer_, NA_integer_)} \tab \code{NA} \cr
#' }
#' 
#' @param x A list of integer vectors.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#'
#' @examples
#' x <- list(c(20L, 21L), c(80L, NA), c(11L, 12L))
#' make_labels_integers(x)
#' @keywords internal
#' @export
make_labels_integers <- function(x) {
    demcheck::err_values_integers(x = x,
                                  name = "x")
    low <- vapply(X = x, FUN = `[[`, 1L, FUN.VALUE = 0L)
    up <- vapply(X = x, FUN = `[[`, 2L, FUN.VALUE = 0L)
    n <- length(x)
    ans <- rep(NA_character_, length = n)
    is_na_low <- is.na(low)
    is_na_up <- is.na(up)
    is_open_low <- is_na_low & !is_na_up
    is_open_up <- !is_na_low & is_na_up
    is_low_up <- !is_na_low & !is_na_up
    ans[is_open_low] <- paste0("<", up[is_open_low])
    ans[is_open_up] <- paste0(low[is_open_up], "+")
    ans[is_low_up] <- low[is_low_up]
    ans
}


## HAS_TESTS
#' Make Intervals labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Intervals"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of integer vectors of length 2.
#' If elements of a vector are non-NA, then the second
#' element must be greater than the first.
#' The vectors cannot overlap.
#'
#' The elements of \code{x} are converted into
#' labels as follows:
#'
#' \tabular{ll}{
#'   Element \tab Label \cr
#'   \code{c(100L, 101L)} \tab \code{"100-101"} \cr
#'   \code{c(100L, 110L)} \tab \code{"100-110"} \cr
#'   \code{c(NA_integer_, 100L)} \tab \code{"<100"} \cr
#'   \code{c(100L, NA_integer_)} \tab \code{"100+"} \cr
#'   \code{c(NA_integer_, NA_integer_)} \tab \code{NA} \cr
#' }
#' 
#' @param x A list of integer vectors.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#' 
#' @examples
#' x <- list(c(2000L, 2010L),
#'           c(2011L, 2012L),
#'           c(NA, 2000L),
#'           c(NA_integer_, NA__integer_),
#'           c(2030L, NA),
#'           c(2020L, 2030L))
#' make_labels_intervals(x)
#' @keywords internal
#' @export
make_labels_intervals <- function(x) {
    demcheck::err_values_intervals(x = x,       
                                   name = "x")  
    low <- vapply(X = x, FUN = `[[`, 1L, FUN.VALUE = 0L)
    up <- vapply(X = x, FUN = `[[`, 2L, FUN.VALUE = 0L)
    n <- length(x)
    ans <- rep(NA_character_, length = n)
    is_na_low <- is.na(low)
    is_na_up <- is.na(up)
    is_open_low <- is_na_low & !is_na_up
    is_open_up <- !is_na_low & is_na_up
    is_low_up <- !is_na_low & !is_na_up
    ans[is_open_low] <- paste0("<", up[is_open_low])
    ans[is_open_up] <- paste0(low[is_open_up], "+")
    ans[is_low_up] <- paste0(low[is_low_up], "-", up[is_low_up])
    ans
}


## HAS_TESTS
#' Make Quantities labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Quantities"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of integer vectors of length 2.
#' If elements of a vector are non-NA, then the second
#' element must be greater than the first.
#' The vectors cannot overlap.
#'
#' The elements of \code{x} are converted into
#' labels as follows:
#'
#' \tabular{ll}{
#'   Element \tab Label \cr
#'   \code{c(100L, 101L)} \tab \code{"100"} \cr
#'   \code{c(100L, 110L)} \tab \code{"100-109"} \cr
#'   \code{c(NA_integer_, 100L)} \tab \code{"<100"} \cr
#'   \code{c(100L, NA_integer_)} \tab \code{"100+"} \cr
#'   \code{c(NA_integer_, NA_integer_)} \tab \code{NA} \cr
#' }
#' 
#' @param x A list of integer vectors.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#' 
#' @examples
#' x <- list(c(0L, 1L),
#'           c(1L, 5L),
#'           c(NA, 0L),
#'           c(NA_integer_, NA__integer_),
#'           c(20L, NA),
#'           c(10L, 20L))
#' make_labels_quantities(x)
#' @keywords internal
#' @export
make_labels_quantities <- function(x) {
    demcheck::err_values_quantities(x = x,
                                    name = "x")
    low <- vapply(X = x, FUN = `[[`, 1L, FUN.VALUE = 0L)
    up <- vapply(X = x, FUN = `[[`, 2L, FUN.VALUE = 0L)
    n <- length(x)
    ans <- rep(NA_character_, length = n)
    is_na_low <- is.na(low)
    is_na_up <- is.na(up)
    is_open_low <- is_na_low & !is_na_up
    is_open_up <- !is_na_low & is_na_up
    is_low_up <- !is_na_low & !is_na_up & (up - low > 1L)
    is_single <- !is_na_low & !is_na_up & (up - low == 1L)
    ans[is_open_low] <- paste0("<", up[is_open_low])
    ans[is_open_up] <- paste0(low[is_open_up], "+")
    ans[is_low_up] <- paste0(low[is_low_up], "-", up[is_low_up] - 1L)
    ans[is_single] <- low[is_single]
    ans
}


## HAS_TESTS
#' Make Quarters labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Quarters"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of Date vectors of length 2.
#' If elements of a vector are non-NA, then the second
#' element must one quarter after the first.
#' Any non-NA elements must be the first day of a quarter.
#' The vectors cannot overlap.
#'
#' The elements of \code{x} are converted into
#' labels as follows:
#'
#' \tabular{ll}{
#'   Element \tab Label \cr
#'   \code{as.Date(c("2020-01-01", "2020-04-01"))} \tab \code{"2020 Q1"} \cr
#'   \code{as.Date(c(NA, "2020-01-01"))} \tab \code{"<2020 Q1"} \cr
#'   \code{as.Date(c("2020-01-01", NA))} \tab \code{"2020 Q1+"} \cr
#'   \code{as.Date(c(NA, NA))} \tab \code{NA} \cr
#' }
#'
#' @param x A list of Date vectors.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#' 
#' @examples
#' x <- list(as.Date(c("2020-10-01", "2021-01-01")),
#'           as.Date(c(NA, "2020-01-01")),
#'           as.Date(c(NA_character_, NA__character_)),
#'           as.Date(c("2025-01-01", NA)),
#'           as.Date(c("2020-01-01", "2020-04-01")))
#' make_labels_quarters(x)
#' @keywords internal
#' @export
make_labels_quarters <- function(x) {
    demcheck::err_values_quarters(x = x,
                                  name = "x")
    ## can't easily extract first and second elements
    ## as vectors, since R will coerce to numeric
    n <- length(x)
    ans <- character(length = n)
    for (i in seq_len(n)) {
        item <- x[[i]]
        low <- item[[1L]]
        up <- item[[2L]]
        is_na_low <- is.na(low)
        is_na_up <- is.na(up)
        year_low <- format(low, format = "%Y")
        quarter_low <- quarters(low)
        year_up <- format(up, format = "%Y")
        quarter_up <- quarters(up)
        is_open_first <- is_na_low && !is_na_up
        is_open_last <- !is_na_low && is_na_up
        is_low_up <- !is_na_low && !is_na_up
        if (is_open_first)
            ans[[i]] <- sprintf("<%s %s", year_up, quarter_up)
        else if (is_open_last)
            ans[[i]] <- sprintf("%s %s+", year_low, quarter_low)
        else if (is_low_up)
            ans[[i]] <- sprintf("%s %s", year_low, quarter_low)
        else
            ans[[i]] <- NA_character_
        
    }
    ans
}


## HAS_TESTS
#' Make Months labels
#'
#' Make labels with the format expected for an
#' object of class \code{"Months"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of Date vectors of length 2.
#' If elements of a vector are non-NA, then the second
#' element must one month after the first.
#' Any non-NA elements must be the first day of a month.
#' The vectors cannot overlap.
#'
#' The elements of \code{x} are converted into
#' labels as follows:
#'
#' \tabular{ll}{
#'   Element \tab Label \cr
#'   \code{as.Date(c("2020-01-01", "2020-01-01"))} \tab \code{"2020 Jan"} \cr
#'   \code{as.Date(c(NA, "2020-01-01"))} \tab \code{"<2020 Jan"} \cr
#'   \code{as.Date(c("2020-01-01", NA))} \tab \code{"2020 Jan+"} \cr
#'   \code{as.Date(c(NA, NA))} \tab \code{NA} \cr
#' }
#'
#' @param x A list of Date vectors.
#'
#' @return A character vector
#' 
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_dateranges}},
#' \code{\link{make_labels_datepoints}}
#'
#' @examples
#' x <- list(as.Date(c("2020-12-01", "2021-01-01")),
#'           as.Date(c(NA, "2020-01-01")),
#'           as.Date(c(NA_character_, NA__character_)),
#'           as.Date(c("2025-01-01", NA)),
#'           as.Date(c("2020-01-01", "2020-02-01")))
#' make_labels_months(x)
#' @keywords internal
#' @export
make_labels_months <- function(x) {
    demcheck::err_values_months(x = x,
                                name = "x")
    ## can't easily extract first and second elements
    ## as vectors, since R will coerce to numeric
    n <- length(x)
    ans <- character(length = n)
    for (i in seq_len(n)) {
        item <- x[[i]]
        low <- item[[1L]]
        up <- item[[2L]]
        is_na_low <- is.na(low)
        is_na_up <- is.na(up)
        year_month_low <- format(low, format = "%Y %b")
        year_month_up <- format(up, format = "%Y %b")
        is_open_first <- is_na_low && !is_na_up
        is_open_last <- !is_na_low && is_na_up
        is_low_up <- !is_na_low && !is_na_up
        if (is_open_first)
            ans[[i]] <- paste0("<", year_month_up)
        else if (is_open_last)
            ans[[i]] <- paste0(year_month_low, "+")
        else if (is_low_up)
            ans[[i]] <- year_month_low
        else
            ans[[i]] <- NA_character_
    }
    ans
}


## HAS_TESTS
#' Make DateRanges labels
#'
#' Make labels with the format expected for an
#' object of class \code{"DateRanges"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of Date vectors of length 2.
#' If elements of a vector are non-NA, then the second
#' element must be later than the first.
#'
#' The elements of \code{x} are converted into
#' labels as follows:
#'
#' \tabular{ll}{
#'   Element \tab Label \cr
#'   \code{as.Date(c("2020-01-15", "2020-02-11"))} \tab \code{"[2020-01-15, 2020-02-10]"} \cr
#'   \code{as.Date(c(NA, "2020-01-01"))} \tab \code{"(-Inf, 2019-12-31]"} \cr
#'   \code{as.Date(c("2020-01-01", NA))} \tab \code{"[2020-01-01, Inf)"} \cr
#'   \code{as.Date(c(NA, NA))} \tab \code{NA} \cr
#' }
#'
#' @param x A list of Date vectors.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_datepoints}}
#' 
#' @examples
#' x <- list(as.Date(c("2020-10-15", "2021-02-28")),
#'           as.Date(c(NA, "2020-01-31")),
#'           as.Date(c(NA_character_, NA__character_)),
#'           as.Date(c("2025-01-01", NA)),
#'           as.Date(c("2023-01-01", "2023-01-01")))
#' make_labels_dateranges(x)
#' @keywords internal
#' @export
make_labels_dateranges <- function(x) {
    demcheck::err_values_dateranges(x = x,
                                    name = "x")
    ## can't easily extract first and second elements
    ## as vectors, since R will coerce to numeric
    n <- length(x)
    ans <- character(length = n)
    for (i in seq_len(n)) {
        item <- x[[i]]
        low <- item[[1L]]
        up <- item[[2L]]
        is_na_low <- is.na(low)
        is_na_up <- is.na(up)
        is_open_first <- is_na_low && !is_na_up
        is_open_last <- !is_na_low && is_na_up
        is_low_up <- !is_na_low && !is_na_up
        if (is_open_first)
            ans[[i]] <- sprintf("(-Inf, %s]", up - 1L)
        else if (is_open_last)
            ans[[i]] <- sprintf("[%s, Inf)", low)
        else if (is_low_up)
            ans[[i]] <- sprintf("[%s, %s]", low, up - 1L)
        else
            ans[[i]] <- NA_character_
    }
    ans
}


## HAS_TESTS
#' Make DatePoints labels
#'
#' Make labels with the format expected for an
#' object of class \code{"DatePoints"}
#' (as defined in the demarray package).
#' This function would not normally be called directly
#' by end users.
#'
#' @param x A Date vector.
#'
#' @return A character vector
#'
#' @seealso \code{\link{make_labels_categories}},
#' \code{\link{make_labels_triangles}},
#' \code{\link{make_labels_directions}},
#' \code{\link{make_labels_quantiles}},
#' \code{\link{make_labels_integers}},
#' \code{\link{make_labels_intervals}},
#' \code{\link{make_labels_quantities}},
#' \code{\link{make_labels_quarters}},
#' \code{\link{make_labels_months}},
#' \code{\link{make_labels_dateranges}}
#' 
#' @examples
#' x <- as.Date(c("2020-07-01", "2015-07-01", NA))
#' make_labels_datepoints(x)
#' @keywords internal
#' @export
make_labels_datepoints <- function(x) {
    demcheck::err_values_datepoints(x = x,
                                    name = "x")
    ans <- as.character(x)
    ans
}

