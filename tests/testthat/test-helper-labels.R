
context("helper-labels")


## breaks_to_pairs_integer ----------------------------------------------------

test_that("'breaks_to_pairs_integer' gives correct answer with valid inputs", {
    ans_obtained <- breaks_to_pairs_integer(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = TRUE,
                                            open_last = TRUE,
                                            include_na = TRUE)
    ans_expected <- list(c(NA, 0L),
                         c(0L, 1L),
                         c(1L, 5L),
                         c(5L, 10L),
                         c(10L, NA),
                         c(NA_integer_, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs_integer(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = FALSE,
                                            open_last = TRUE,
                                            include_na = TRUE)
    ans_expected <- list(c(0L, 1L),
                         c(1L, 5L),
                         c(5L, 10L),
                         c(10L, NA),
                         c(NA_integer_, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs_integer(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = TRUE)
    ans_expected <- list(c(0L, 1L),
                         c(1L, 5L),
                         c(5L, 10L),
                         c(NA_integer_, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs_integer(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = FALSE)
    ans_expected <- list(c(0L, 1L),
                         c(1L, 5L),
                         c(5L, 10L))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs_integer(breaks = integer(),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = FALSE)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs_integer(breaks = integer(),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = TRUE)
    ans_expected <- list(c(NA_integer_, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs_integer(breaks = 0L,
                                            open_first = TRUE,
                                            open_last = TRUE,
                                            include_na = FALSE)
    ans_expected <- list(c(NA_integer_, 0L),
                         c(0L, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'breaks_to_pairs_integer' raises expected error with invalid inputs", {
    expect_error(breaks_to_pairs_integer(breaks = integer(),
                                         open_first = TRUE,
                                         open_last = FALSE,
                                         include_na = FALSE),
                 "'breaks' has length 0 but 'open_first' is TRUE")
    expect_error(breaks_to_pairs_integer(breaks = integer(),
                                         open_first = FALSE,
                                         open_last = TRUE,
                                         include_na = FALSE),
                 "'breaks' has length 0 but 'open_last' is TRUE")
    expect_error(breaks_to_pairs_integer(breaks = 2000L,
                                         open_first = FALSE,
                                         open_last = FALSE,
                                         include_na = FALSE),
                 "'breaks' has length 1 but 'open_first' and 'open_last' are both FALSE")
})
