
context("clean")

## clean_age ------------------------------------------------------------------

test_that("'clean_age' works with mix of valid and invalid labels", {
    x <- c("0 Year", "1 to 4 Years", "wrong", "10 Years And Over")
    ans_obtained <- clean_age(x)
    ans_expected <- c("0", "1-4", "wrong", "10+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("0 yr", "1--4 yrs", "5--9 yrs", NA)
    ans_obtained <- clean_age(x)
    ans_expected <- c("0", "1-4", "5-9", NA)
    expect_identical(ans_obtained, ans_expected)
    x <- c("lesinfants", "one", "two", "three")
    ans_obtained <- clean_age(x)
    ans_expected <- c("lesinfants", "1", "2", "3")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'clean_age' works with multiples of 5", {
    x <- seq(50, 0, -5)
    ans_obtained <- clean_age(x)
    ans_expected <- c("50+",
                      paste(seq(45, 0, -5), seq(49, 4, -5), sep = "-"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'clean_age' works with lifetable age groups", {
    x <- c(1,
           seq(50, 0, -5))
    ans_obtained <- clean_age(x)
    ans_expected <- c("1-4",
                      "50+",
                      paste(seq(45, 5, -5), seq(49, 9, -5), sep = "-"),
                      "0")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'clean_age' works with vectors with length 0", {
    expect_identical(clean_age(integer()),
                     character())
})


## clean_age_df ---------------------------------------------------------------

test_that("clean_age_df works", {
    x <- c("10 Years And Over", "0 Year", "wrong", "5 to 9 Years", "0 Year")
    input <- c("10 Years And Over", "0 Year", "wrong", "5 to 9 Years")
    output <- c("10+", "0", "wrong", "5-9")
    is_valid <- c(TRUE, TRUE, FALSE, TRUE)
    ans_obtained <- clean_age_df(x)
    ans_expected <- data.frame(input = input,
                               output = output,
                               is_valid = is_valid)
    expect_identical(ans_obtained, ans_expected)
})


## clean_cohort ---------------------------------------------------------------

test_that("'clean_cohort' works with mix of valid and invalid labels", {
    x <- c("2020", "2020 to 2025", "<February 2021", "third quarter 2022", NA)
    expect_identical(clean_cohort(x),
                     c("2020", "2020-2025", "<2021 Feb", "2022 Q3", NA))
    x <- c("up to 2020", "2020--2025", "less than Jun 2021", "wrong", "")
    expect_identical(clean_cohort(x),
                     c("<2020", "2020-2025", "<2021 Jun", "wrong", ""))
    x <- c("q2 2020", "January 2025", "2021-February", "before second quarter 2020")
    expect_identical(clean_cohort(x),
                     c("2020 Q2", "2025 Jan", "2021 Feb", "<2020 Q2"))
})
    
    
