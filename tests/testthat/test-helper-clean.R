
context("helper-clean")


## clean_age_5 ----------------------------------------------------------------

test_that("'clean_age_5' returns cleaned 'x' when 'x' denotes 5-year age groups", {
    clean_age_5 <- demprep:::clean_age_5
    ## no NAs
    x <- seq(0, 100, 5)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    ans_obtained <- clean_age_5(x)
    ans_expected <- paste(x, x+4, sep = "-")
    ans_expected[x == 100] <- "100+"
    expect_identical(ans_obtained, ans_expected)
    ## with NAs, is character
    x <- c(seq(0, 50, 5), NA)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x_int <- x
    x <- as.character(x)
    ans_obtained <- clean_age_5(x)
    ans_expected <- x
    lt_50 <- !is.na(x) & (x_int < 50)
    is_50 <- !is.na(x) & (x_int == 50)
    ans_expected[lt_50] <- paste(x_int[lt_50], x_int[lt_50] + 4, sep = "-")
    ans_expected[is_50] <- "50+"
    expect_identical(ans_obtained, ans_expected)
})

test_that("'clean_age_5' returns NULL when 'x' does not denote 5-year age groups", {
    clean_age_5 <- demprep:::clean_age_5
    ## length 0
    expect_null(clean_age_5(character()))
    ## no NAs
    x <- seq(0, 100, 5)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x[[10L]] <- "wrong"
    expect_null(clean_age_5(x))
    ## with NAs, is character
    x <- c(seq(0, 50, 5), NA)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x <- as.character(x)
    x[[10L]] <- "wrong"
    expect_null(clean_age_5(x))
})


## clean_age_guess ------------------------------------------------------------

test_that("'clean_age_guess' correctly interprets valid labels", {
    clean_age_guess <- demprep:::clean_age_guess
    x <- c("0 Year", "1 to 4 Years", "5 to 9 Years", "10 Years And Over")
    ans_obtained <- clean_age_guess(x, language = "English")
    ans_expected <- c("0", "1-4", "5-9", "10+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("0 yr", "1--4 yrs", "5--9 yrs", "10plus")
    ans_obtained <- clean_age_guess(x, language = "English")
    ans_expected <- c("0", "1-4", "5-9", "10+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("infants", "one", "two", "three")
    ans_obtained <- clean_age_guess(x, language = "English")
    ans_expected <- c("0", "1", "2", "3")
    expect_identical(ans_obtained, ans_expected)
    x <- c("00", "01.04", "05.09", "10.14")
    ans_obtained <- clean_age_guess(x, language = "English")
    ans_expected <- c("0", "1-4", "5-9", "10-14")
    expect_identical(ans_obtained, ans_expected)
    x <- c("one month", "2 months", "zero months", "100 m and over")
    ans_obtained <- clean_age_guess(x, language = "English")
    ans_expected <- c("1m", "2m", "0m", "100m+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("11 qtrs", "five quarters or more", "0 qu", "100  quarter")
    ans_obtained <- clean_age_guess(x, language = "English")
    ans_expected <- c("11q", "5q+", "0q", "100q")
    expect_identical(ans_obtained, ans_expected)
})


## clean_age_lifetab ----------------------------------------------------------

test_that("'clean_age_lifetab' returns cleaned 'x' when 'x' denotes 5-year age groups", {
    clean_age_lifetab <- demprep:::clean_age_lifetab
    ## no NAs
    x <- c(1L, seq(0, 100, 5))
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    ans_obtained <- clean_age_lifetab(x)
    ans_expected <- paste(x, x+4, sep = "-")
    ans_expected[x == 0] <- "0"
    ans_expected[x == 1] <- "1-4"
    ans_expected[x == 100] <- "100+"
    expect_identical(ans_obtained, ans_expected)
    ## with NAs, is character
    x <- c(1L, seq(0, 50, 5), NA)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x_int <- x
    x <- as.character(x)
    ans_obtained <- clean_age_lifetab(x)
    ans_expected <- x
    is_0 <- !is.na(x) & (x_int == 0)
    is_1 <- !is.na(x) & (x_int == 1)
    is_mid <- !is.na(x) & (x_int > 1) & (x_int < 50)
    is_50 <- !is.na(x) & (x_int == 50)
    ans_expected[is_0] <- "0"
    ans_expected[is_1] <- "1-4"
    ans_expected[is_mid] <- paste(x_int[is_mid], x_int[is_mid] + 4, sep = "-")
    ans_expected[is_50] <- "50+"
    expect_identical(ans_obtained, ans_expected)
})

test_that("'clean_age_lifetab' returns NULL when 'x' does not denote life table age groups", {
    clean_age_lifetab <- demprep:::clean_age_lifetab
    ## length 0
    expect_null(clean_age_lifetab(character()))
    ## no NAs
    x <- c(1, seq(0, 100, 5))
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x[[10L]] <- "wrong"
    expect_null(clean_age_lifetab(x))
    ## with NAs, is character
    x <- c(1, seq(0, 50, 5), NA)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x <- as.character(x)
    x[[10L]] <- "wrong"
    expect_null(clean_age_lifetab(x))
})


