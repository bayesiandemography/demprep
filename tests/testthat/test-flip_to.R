
test_that("'flip_to_end' works with 'month_start' equal to 'Jan'", {
    x <- 2000L
    ans_obtained <- flip_to_end(x, month_start = "Jan")
    ans_expected <- 2000L
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c("2000", "<2000"))
    ans_obtained <- flip_to_end(x, month_start = "Jan")
    ans_expected <- factor(x, levels = levels(x))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- flip_to_end(factor(NA, exclude = NULL), month_start = "Jan")
    ans_expected <- factor(as.character(NA), exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'flip_to_end' works with 'month_start' not equal to 'Jan'", {
    x <- "2000"
    ans_obtained <- flip_to_end(x, month_start = "Feb")
    ans_expected <- "2001"
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- flip_to_end(factor("<2002"), month_start = "Feb")
    ans_expected <- factor("<2003")
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- flip_to_end(factor(NA, exclude = NULL), month_start = "Feb")
    ans_expected <- factor(NA, exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'flip_to_start' works with 'month_start' equal to 'Jan'", {
    x <- "2000"
    ans_obtained <- flip_to_start(x, month_start = "Jan")
    ans_expected <- "2000"
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c("2000", "<2000"))
    ans_obtained <- flip_to_start(x, month_start = "Jan")
    ans_expected <- factor(x, levels = levels(x))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- flip_to_start(NA, month_start = "Jan")
    ans_expected <- NA
    expect_identical(ans_obtained, ans_expected)
})

test_that("'flip_to_start' works with 'month_start' not equal to 'Jan'", {
    x <- "2000"
    ans_obtained <- flip_to_start(x, month_start = "Feb")
    ans_expected <- "1999"
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- flip_to_start(factor("<2002"), month_start = "Feb")
    ans_expected <- factor("<2001")
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- flip_to_start(NA, month_start = "Feb")
    ans_expected <- NA
    expect_identical(ans_obtained, ans_expected)
})
          
