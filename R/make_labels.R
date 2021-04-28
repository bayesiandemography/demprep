
## NO_TESTS
#' Make Categories labels
#'
#' Make labels with the format expected for a \code{"Categories"}
#' dimlabels object (as defined in the demarray package), or for
#' the \code{"Triangles"}, \code{"Directions"},
#' and \code{"Quantiles"} subclasses of \code{"Categories"}.
#' 
#' These functions would not normally be called directly
#' by end users.
#'
#' \code{x} is a character vector, with no duplicates or blanks.
#' With \code{make_labels_triangles}, the elements must be
#' \code{"Lower"}, \code{"Upper"}, or \code{NA}.
#' With \code{make_labels_directions}, the elements must be
#' \code{"In"}, \code{"Out"}, or \code{NA}.
#' With \code{make_labels_quantiles}, the elements must be
#' a number between 0 and 100, followed by a "%" sign,
#' or be an \code{NA}.
#' 
#' @param x A character vector.
#'
#' @return A character vector
#'
#' @examples
#' make_labels_categories(x = c("Nigeria", "Ethiopia", NA, "South Africa"))
#' make_labels_triangles(x = c("Upper", NA, "Lower"))
#' make_labels_directions(x = c("In", NA, "Out"))
#' make_labels_quantiles(x = c("97.5%", NA, "2.5%", "50%"))
#' @keywords internal
#' @name make_labels_categories
NULL

#' @rdname make_labels_categories
#' @export
make_labels_categories <- function(x) {
    demcheck::err_values_categories(x = x,
                                    name = "x")
    x
}

#' @rdname make_labels_categories
#' @export
make_labels_triangles <- function(x) {
    demcheck::chk_values_triangles(x = x,
                                   name = "x")
    x
}

#' @rdname make_labels_categories
#' @export
make_labels_directions <- function(x) {
    demcheck::err_values_directions(x = x,
                                    name = "x")
    x
}

#' @rdname make_labels_categories
#' @export
make_labels_quantiles <- function(x) {
    demcheck::err_values_quantiles(x = x,
                                   name = "x")
    x
}


## NO_TESTS
#' Make Integers labels
#'
#' Make labels with the format expected for a \code{"Integers"}
#' dimlabels object (as defined in the demarray package.)
#' 
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is an integer vector with no duplicates.
#' 
#' @param x An integer vector.
#'
#' @return A character vector
#'
#' @examples
#' x <- list(c(20L, 21L), c(80L, NA), c(11L, 12L))
#' make_labels_integers(x)
#' @keywords internal
#' @export
make_labels_integers <- function(x) {
    demcheck::err_values_integers(x = x,
                                  name = "x")
    low <- vapply(X = x, FUN = `[[`, 1L, FUN.VALUE = 1L)
    up <- vapply(X = x, FUN = `[[`, 2L, FUN.VALUE = 1L)
    n <- length(x)
    ans <- rep(NA_character_, length = n)
    is_na_low <- is.na(low)
    is_na_up <- is.na(up)
    is_open_low <- is_na_low & !is_na_up
    is_open_up <- !is_na_low & is_na_up
    is_low_up <- !is_na_low & !is_na_up
    ans[is_open_low] <- paste0("<", up[is_open_low])
    ans[is_open_up] <- paste0(low[is_open_up], "+")
    ans[is_low_up] <- low
    ans
}


## NO_TESTS
#' Make Intervals labels
#'
#' Make labels with the format expected for a \code{"Intervals"}
#' dimlabels object (as defined in the demarray package.)
#' 
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of integer vectors of
#' length 2. When both elements in an item are
#' non-NA, the second element must be greater than
#' the first. Intervals must not overlap.
#' 
#' @param x A list.
#'
#' @return A character vector
#'
#' @examples
#' x <- list(c(2000L, 2010L),
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
    low <- vapply(X = x, FUN = `[[`, 1L, FUN.VALUE = 1L)
    up <- vapply(X = x, FUN = `[[`, 2L, FUN.VALUE = 1L)
    n <- length(x)
    ans <- rep(NA_character_, length = n)
    is_na_low <- is.na(low)
    is_na_up <- is.na(up)
    is_open_low <- is_na_low & !is_na_up
    is_open_up <- !is_na_low & is_na_up
    is_low_up <- !is_na_low & !is_na_up
    ans[is_open_low] <- paste0("<", up[is_open_low])
    ans[is_open_up] <- paste0(low[is_open_up], "+")
    ans[is_low_up] <- paste0(low, "-", up)
    ans
}


## NO_TESTS
#' Make Quantities labels
#'
#' Make labels with the format expected for a \code{"Quantities"}
#' dimlabels object (as defined in the demarray package.)
#' 
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of integer vectors of
#' length 2. When both elements in an item are
#' non-NA, the second element must be greater than
#' the first. Quantities must not overlap.
#' 
#' @param x A list.
#'
#' @return A character vector
#'
#' @examples
#' x <- list(c(0L, 4L),
#'           c(NA, -1L),
#'           c(NA_integer_, NA__integer_),
#'           c(20L, NA),
#'           c(10L, 19L))
#' make_labels_quantities(x)
#' @keywords internal
#' @export
make_labels_quantities <- function(x) {
    demcheck::err_values_quantities(x = x,
                                    name = "x")
    low <- vapply(X = x, FUN = `[[`, 1L, FUN.VALUE = 1L)
    up <- vapply(X = x, FUN = `[[`, 2L, FUN.VALUE = 1L)
    n <- length(x)
    ans <- rep(NA_character_, length = n)
    is_na_low <- is.na(low)
    is_na_up <- is.na(up)
    is_open_low <- is_na_low & !is_na_up
    is_open_up <- !is_na_low & is_na_up
    is_low_up <- !is_na_low & !is_na_up
    is_single <- is_low_up & (up - low == 1L)
    ans[is_open_low] <- paste0("<", up[is_open_low] + 1L)
    ans[is_open_up] <- paste0(low[is_open_up], "+")
    ans[is_low_up] <- paste0(low, "-", up)
    ans[single] <- low[is_single]
    ans
}


## NO_TESTS
#' Make Quarters labels
#'
#' Make labels with the format expected for a \code{"Quarters"}
#' dimlabels object (as defined in the demarray package.)
#' 
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of date vectors of
#' length 2. When both elements in an item are
#' non-NA, the second element must be one quarter
#' later than the first. Quarters must not overlap.
#' 
#' @param x A list.
#'
#' @return A character vector
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
    ans <- rep(NA_character_, length = n)
    for (i in seq_len(n)) {
        item <- x[[i]]
        low <- item[[1L]]
        up <- x[[2L]]
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
        else
            ans[[i]] <- sprintf("%s %s", year_low, quarter_low)
    }
    ans
}


## NO_TESTS
#' Make Months labels
#'
#' Make labels with the format expected for a \code{"Months"}
#' dimlabels object (as defined in the demarray package.)
#' 
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of date vectors of
#' length 2. When both elements in an item are
#' non-NA, the second element must be one month
#' greater than the first. Months must not overlap.
#' 
#' @param x A list.
#'
#' @return A character vector
#'
#' @examples
#' x <- list(as.Date(c("2020-10-01", "2020-11-01")),
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
    ans <- rep(NA_character_, length = n)
    for (i in seq_len(n)) {
        item <- x[[i]]
        low <- item[[1L]]
        up <- x[[2L]]
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
        else
            ans[[i]] <- year_month_low
    }
    ans
}


## NO_TESTS
#' Make DatePoints labels
#'
#' Make labels with the format expected for a \code{"DatePoints"}
#' dimlabels object (as defined in the demarray package.)
#' 
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a dates vector with no duplicates.
#' 
#' @param x A vector of class \code{\link[base]{Date}}.
#'
#' @return A character vector
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


## NO_TESTS
#' Make DateRanges labels
#'
#' Make labels with the format expected for a \code{"DateRanges"}
#' dimlabels object (as defined in the demarray package.)
#' 
#' This function would not normally be called directly
#' by end users.
#'
#' \code{x} is a list of date vectors of
#' length 2. When both elements in an item are
#' non-NA, the second element must later than
#' the first. Ranges must not overlap.
#' 
#' @param x A list.
#'
#' @return A character vector
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
make_labels_quarters <- function(x) {
    demcheck::err_values_quarters(x = x,
                                  name = "x")
    ## can't easily extract first and second elements
    ## as vectors, since R will coerce to numeric
    n <- length(x)
    ans <- rep(NA_character_, length = n)
    for (i in seq_len(n)) {
        item <- x[[i]]
        low <- item[[1L]]
        up <- x[[2L]]
        is_na_low <- is.na(low)
        is_na_up <- is.na(up)
        is_open_first <- is_na_low && !is_na_up
        is_open_last <- !is_na_low && is_na_up
        is_low_up <- !is_na_low && !is_na_up
        if (is_open_first)
            ans[[i]] <- sprintf("(-Inf, %s]", up - 1L)
        else if (is_open_last)
            ans[[i]] <- sprintf("[%s, Inf)", low)
        else
            ans[[i]] <- sprintf("[%s, %s]", low, up - 1L)
    }
    ans
}
