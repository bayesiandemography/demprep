
## HAS_TESTS
breaks_to_pairs <- function(breaks, open_first, open_last, include_na) {
    if (is.integer(breaks))
        na <- NA_integer_
    else if (inherits(breaks, "Date"))
        na <- as.Date(NA)
    else
        stop(gettextf("'%s' has class \"%s\"",
                      "breaks", class(breaks)),
             call. = FALSE)
    n <- length(breaks)
    if (n == 0L) {
        if (open_first)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_first", TRUE),
                 call. = FALSE)
        if (open_last)
            stop(gettextf("'%s' has length %d but '%s' is %s",
                          "breaks", 0L, "open_last", TRUE),
                 call. = FALSE)
        ans <- list()
    }
    else if (n == 1L) {
        if (!open_first && !open_last)
            stop(gettextf("'%s' has length %d but '%s' and '%s' are both %s",
                          "breaks", 1L, "open_first", "open_last", "FALSE"),
                 call. = FALSE)
        ans <- list()
    }
    else {
        low <- breaks[-n]
        up <- breaks[-1L]
        ans <- mapply(FUN = c, low, up, SIMPLIFY = FALSE)
    }
    if (open_first) {
        x_first <- c(na, breaks[[1L]])
        x_first <- list(x_first)
        ans <- c(x_first, ans)
    }
    if (open_last) {
        x_last <- c(breaks[[n]], na)
        x_last <- list(x_last)
        ans <- c(ans, x_last)
    }
    if (include_na) {
        x_na <- c(na, na)
        x_na <- list(x_na)
        ans <- c(ans, x_na)
    }
    ans
}








