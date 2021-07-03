
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
        infant <- "^infants$|^infant$|^in 1st$|^less than 1$|^under 1$|^less than one$"
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
    ## return result
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


## HAS_TESTS
## If 'x' consists of possible cohort labels
## following non-dem formats,
## apply a standard set of transformations
## that are likely to turn the elements of 'x'
## into valid dem formats
clean_cohort_period_guess <- function(x, language, open_first) {
    if (language == "English") {
        lessthan <- "less than|up to|before"
        quarters <- list(Q1 = "first quarter|quarter ?1|qu ?1|q ?1",
                         Q2 = "second quarter|quarter ?2|qu ?2|q ?2",
                         Q3 = "third quarter|quarter ?3|qu ?3|q ?3",
                         Q4 = "fourth quarter|quarter ?4|qu ?4|q ?4")
        months <- list(Jan = "january|jan",
                       Feb = "february|feb",
                       Mar = "march|mar",
                       Apr = "april|apr",
                       May = "may|may",
                       Jun = "june|jun",
                       Jul = "july|jul",
                       Aug = "august|aug",
                       Sep = "september|sep",
                       Oct = "october|oct",
                       Nov = "november|nov",
                       Dec = "december|dec")
    }
    else
        stop(gettextf("cannot process language \"%s\"",
                      language),
             call. = FALSE)
    ## put everything into lower case
    x <- tolower(x)
    ## trim leading zeros from any numbers
    x <- gsub("(?<![0-9])0+(?=[0-9])", "", x, perl = TRUE)
    ## trim leading and trailing spaces
    x <- gsub("^ +| +$", "", x)
    ## translate quarters
    for (i in seq_along(quarters)) {
        ## replace synonym with official quarter name
        synonyms <- quarters[[i]]
        name <- names(quarters)[[i]]
        ## reformat where quarter and year in wrong order
        pattern <- sprintf("(%s)[- ._]+([0-9]+)$", synonyms)
        replacement <- sprintf("\\2 %s", name)
        x <- sub(pattern, replacement, x)
        ## reformat where quarter and year in right order
        pattern <- sprintf("([0-9]+)[- ._]*(%s)$", synonyms)
        replacement <- sprintf("\\1 %s", name)
        x <- sub(pattern, replacement, x)
    }
    ## translate months
    for (i in seq_along(months)) {
        ## replace synonym with official month name
        synonyms <- months[[i]]
        name <- names(months)[[i]]
        ## reformat where month and year in wrong order
        pattern <- sprintf("(%s)[- ._]+([0-9]+)$", synonyms)
        replacement <- sprintf("\\2 %s", name)
        x <- sub(pattern, replacement, x)
        ## reformat where month and year in right order
        pattern <- sprintf("([0-9]+)[- ._]*(%s)$", synonyms)
        replacement <- sprintf("\\1 %s", name)
        x <- sub(pattern, replacement, x)
    }
    ## translate synonyms for "<"
    if (open_first) {
        p <- sprintf("^(%s) *", lessthan)
        x <- sub(p, "<", x)
    }
    ## tranlsate synonyms for "-"
    x <- sub("^([0-9]+) *to *([0-9]+)$", "\\1-\\2", x)
    x <- sub("^([0-9]+) *[[:punct:]]+ *([0-9]+)$", "\\1-\\2", x)
    ## return result
    x
}
