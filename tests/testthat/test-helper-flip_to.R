
test_that("flip_to_internal works when 'month_start' is not January and 'to_end' is TRUE", {
    x <- factor(c("<2000", "2000", NA),
                levels = c("<2000", "2000", NA),
                exclude = NULL)
    ans_obtained <- flip_to_internal(x,
                                     month_start = "Jul",
                                     to_end = TRUE)
    ans_expected <- factor(c("<2001", "2001", NA),
                           levels = c("<2001", "2001", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c(NA, "<2000", "2000")
    ans_obtained <- flip_to_internal(x,
                                     month_start = "Jul",
                                     to_end = TRUE)
    ans_expected <- c(NA, "<2001", "2001")
    expect_identical(ans_obtained, ans_expected)
})

test_that("flip_to_internal works when 'month_start' is not January and 'to_end' is FALSE", {
    x <- factor(c("<2000", "2000", NA),
                levels = c("<2000", "2000", NA),
                exclude = NULL)
    ans_obtained <- flip_to_internal(x,
                                    to_end = FALSE,
                                    month_start = "Jul")
    ans_expected <- factor(c("<1999", "1999", NA),
                           levels = c("<1999", "1999", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2000", "<2000",  NA)
    ans_obtained <- flip_to_internal(x,
                                     to_end = FALSE,
                                     month_start = "Jul")
    ans_expected <- c("1999", "<1999", NA)
    expect_identical(ans_obtained, ans_expected)
})

test_that("flip_to_internal works when 'month_start' is January and 'to_end' is FALSE", {
    x <- factor(c("<2000", "2000", NA),
                levels = c("<2000", "2000", NA),
                exclude = NULL)
    ans_obtained <- flip_to_internal(x,
                                    to_end = FALSE,
                                    month_start = "Jan")
    ans_expected <- factor(c("<2000", "2000", NA),
                           levels = c("<2000", "2000", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("<2000", "2000", NA)
    ans_obtained <- flip_to_internal(x,
                                     to_end = FALSE,
                                     month_start = "Jan")
    ans_expected <- c("<2000", "2000", NA)
    expect_identical(ans_obtained, ans_expected)
})

test_that("flip_to_internal works when 'x' has length 0", {
    x <- factor()
    ans_obtained <- flip_to_internal(x,
                                    to_end = FALSE,
                                    month_start = "Jan")
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- flip_to_internal(x,
                                    to_end = FALSE,
                                    month_start = "Jan")
    ans_expected <- character()
    expect_identical(ans_obtained, ans_expected)
})





