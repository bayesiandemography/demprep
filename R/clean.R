





clean_age <- function(x) {
    ans <- clean_age_5(x)
    if (!is.null(ans))
        return(ans)
    ans <- clean_age_lifetab(x)
    if (!is.null(ans))
        return(ans)
    x <- tolower(x)
    ## trim leading zeros from any numbers
    x <- gsub("(?<![0-9])0+(?=[0-9])", "", x, perl = TRUE)
    ## remove "year" labels
    x <- gsub("year|years|yr|yrs", "", x)
    ## translate synonyms for age group "0"
    x <- gsub("infant|in 1st|less than 1|under 1|less than one", "0", x)
    ## translate synonyms for "+"
    x <- gsub("and over|plus|and above|and older", "+", x)
    ## remove spaces
    x <- gsub(" ", "", x)
    ## tranlsate synonyms for "-"
    x <- gsub("^([0-9]+)to([0-9]+)$", "\\1-\\2", x)
    x <- gsub("^([0-9]+)[[:punct:]]+([0-9]+)$", "\\1-\\2", x)
    ## translate English digits
    x <- gsub("one", "1", x)
    x <- gsub("two", "2", x)
    x <- gsub("three", "3", x)
    x <- gsub("four", "4", x)
    x <- gsub("five", "5", x)
    x <- gsub("six", "6", x)
    x <- gsub("seven", "7", x)
    x <- gsub("eight", "8", x)
    x <- gsub("nine", "9", x)
    x
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
