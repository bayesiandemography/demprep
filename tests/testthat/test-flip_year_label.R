
## flip_year_label ------------------------------------------------------------

test_that("flip_year_label works when 'month_start' is not January and 'current_uses_start' is TRUE", {
    x <- factor(c("<2000", "2000", NA),
                levels = c("<2000", "2000", NA),
                exclude = NULL)
    ans_obtained <- flip_year_label(x,
                                    month_start = "Jul")
    ans_expected <- factor(c("<2001", "2001", NA),
                           levels = c("<2001", "2001", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c(NA, "<2000", "2000")
    ans_obtained <- flip_year_label(x,
                                    month_start = "Jul")
    ans_expected <- factor(c(NA, "<2001", "2001"),
                           levels = c(NA, "<2001", "2001"),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("flip_year_label works when 'month_start' is not January and 'current_uses_start' is FALSE", {
    x <- factor(c("<2000", "2000", NA),
                levels = c("<2000", "2000", NA),
                exclude = NULL)
    ans_obtained <- flip_year_label(x,
                                    current_uses_start = FALSE,
                                    month_start = "Jul")
    ans_expected <- factor(c("<1999", "1999", NA),
                           levels = c("<1999", "1999", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2000", "<2000",  NA)
    ans_obtained <- flip_year_label(x,
                                    current_uses_start = FALSE,
                                    month_start = "Jul")
    ans_expected <- factor(c("1999", "<1999", NA),
                           levels = c("1999", "<1999", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("flip_year_label works when 'month_start' is January and 'current_uses_start' is FALSE", {
    x <- factor(c("<2000", "2000", NA),
                levels = c("<2000", "2000", NA),
                exclude = NULL)
    ans_obtained <- flip_year_label(x,
                                    current_uses_start = FALSE,
                                    month_start = "Jan")
    ans_expected <- factor(c("<2000", "2000", NA),
                           levels = c("<2000", "2000", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("flip_year_label works when 'x' has length 0", {
    x <- factor()
    ans_obtained <- flip_year_label(x,
                                    current_uses_start = FALSE,
                                    month_start = "Jan")
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
})





