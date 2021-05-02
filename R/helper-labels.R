
## HAS_TESTS
breaks_to_pairs_integer <- function(breaks, open_first, open_last, include_na) {
    n <- length(breaks)
    if (n == 0L) {
        if (open_first)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_first", TRUE))
        if (open_last)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_last", TRUE))
        ans <- list()
    }
    else if (n == 1L) {
        if (!open_first && !open_last)
            stop(gettextf("'%s' has length %d but '%s' and '%s' are both %s",
                          "breaks", 1L, "open_first", "open_last", "FALSE"))
        ans <- list()
    }
    else {
        low <- breaks[-n]
        up <- breaks[-1L]
        ans <- mapply(FUN = c, low, up, SIMPLIFY = FALSE)
    }
    if (open_first) {
        x_first <- c(NA_integer_, breaks[[1L]])
        x_first <- list(x_first)
        ans <- c(x_first, ans)
    }
    if (open_last) {
        x_last <- c(breaks[[n]], NA_integer_)
        x_last <- list(x_last)
        ans <- c(ans, x_last)
    }
    if (include_na) {
        x_na <- c(NA_integer_, NA_integer_)
        x_na <- list(x_na)
        ans <- c(ans, x_na)
    }
    ans
}







