
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
