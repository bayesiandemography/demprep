
## HAS_TESTS
## If 'x' consists of integer-like values
## where the unique, sorted
## values form a series 0, 5, 10, ..., A
## (A >= 50), assume that these represent
## age groups "0-4", "5-9", "10-14", ..., "A+";
## otherwise return NULL.
clean_age_5 <- function(x) {
    if (length(x) < 11L)
        return(NULL)
    is_obs <- !is.na(x)
    x_int <- suppressWarnings(as.integer(x))
    if (anyNA(x_int[is_obs]))
        return(NULL)
    if (any(x_int[is_obs] != x[is_obs]))
        return(NULL)
    breaks <- sort(unique(x_int[is_obs]))
    n <- length(breaks)
    if (n < 11L)
        return(NULL)
    s <- seq.int(from = 0L, by = 5L, length.out = n)
    if (!identical(breaks, s))
        return(NULL)
    labels <- paste(breaks, breaks + 4L, sep = "-")
    labels[[n]] <- paste0(breaks[[n]], "+")
    i <- match(x_int, breaks)
    labels[i]
}

## HAS_TESTS
## If 'x' consists of possible age group labels
## following non-dem formats,
## apply a standard set of transformations
## that are likely to turn the elements of 'x'
## into valid dem formats
clean_age_guess <- function(x, language) {
    if (language == "English") {
        year <- "year|years|yr|yrs"
        quarter <- "quarters|quarter|qtrs|qu"
        month <- "months|month|mnths"
        infant <- "^infants$|^in 1st$|^less than 1$|^under 1$|^less than one$"
        plus <- "and over|plus|and above|and older|or more"
        num <- c("zero", "one", "two", "three", "four",
                 "five", "six", "seven", "eight", "nine")
    }
    else
        stop(gettextf("cannot process language \"%s\"",
                      language),
             call. = FALSE)
    ## test whether 'x' consists of lower limits
    ## of 5-year or abridged life table age groups
    ans <- clean_age_5(x)
    if (!is.null(ans))
        return(ans)
    ans <- clean_age_lifetab(x)
    if (!is.null(ans))
        return(ans)
    ## put everything into lower case
    x <- tolower(x)
    ## trim leading zeros from any numbers
    x <- gsub("(?<![0-9])0+(?=[0-9])", "", x, perl = TRUE)
    ## remove "year" labels
    x <- sub(year, "", x)
    ## translate quarters and years
    x <- sub(quarter, "q", x)
    x <- sub(month, "m", x)
    ## translate synonyms for age group "0"
    x <- sub(infant, "0", x)
    ## translate synonyms for "+"
    x <- sub(plus, "+", x)
    ## remove spaces
    x <- gsub(" ", "", x)
    ## tranlsate synonyms for "-"
    x <- sub("^([0-9]+)to([0-9]+)$", "\\1-\\2", x)
    x <- sub("^([0-9]+)[[:punct:]]+([0-9]+)$", "\\1-\\2", x)
    ## translate numbers
    for (i in seq_along(num))
        x <- gsub(num[[i]], i - 1L, x)
    x
}


## HAS_TESTS
## If 'x' consists of integer-like values
## where the unique, sorted
## values form a series 0, 1, 5, 10, ..., A
## (A >= 50), assume that these represent
## lifetable age groups "0", "1-4", "5-9",
## "10-14", ..., "A+";
## otherwise return NULL.
clean_age_lifetab <- function(x) {
    if (length(x) < 12L)
        return(NULL)
    is_obs <- !is.na(x)
    x_int <- suppressWarnings(as.integer(x))
    if (anyNA(x_int[is_obs]))
        return(NULL)
    if (any(x_int[is_obs] != x_int[is_obs]))
        return(NULL)
    breaks <- sort(unique(x_int[is_obs]))
    n <- length(breaks)
    if (n < 12L)
        return(NULL)
    s <- c(0L,
           1L,
           seq.int(from = 5L, by = 5L, length.out = n - 2L))
    if (!identical(breaks, s))
        return(NULL)
    labels <- c("0",
                "1-4",
                paste(breaks[-(1:2)], breaks[-(1:2)] + 4L, sep = "-"))
    labels[[n]] <- paste0(breaks[[n]], "+")
    i <- match(x_int, breaks)
    labels[i]
}


    
