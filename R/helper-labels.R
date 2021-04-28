
## NO_TESTS
breaks_to_values_age <- function(breaks, open_last, include_na) {
    n <- length(breaks)
    low <- breaks[-n]
    up <- breaks[-1L]
    ans <- mapply(FUN = c, low, up, SIMPLIFY = FALSE)
    if (open_last) {
        x_last <- c(up[[n]], NA_integer_)
        ans <- c(ans, list(x_last))
    }
    if (include_na) {
        x_na <- c(NA_integer_, NA_integer_)
        ans <- c(ans, list(x_na))
    }
    ans
}

## NO_TESTS
breaks_to_values_calendar <- function(breaks, open_first, include_na) {
    n <- length(breaks)
    low <- breaks[-n]
    up <- breaks[-1L]
    ans <- mapply(FUN = c, low, up, SIMPLIFY = FALSE)
    if (open_first) {
        x_first <- c(NA_integer_, breaks[[1L]])
        ans <- c(list(x_first), ans)
    }
    if (include_na) {
        x_na <- c(NA_integer_, NA_integer_)
        ans <- c(ans, list(x_na))
    }
    ans
}







