
context("helper-labels")


## breaks_to_pairs ------------------------------------------------------------

test_that("'breaks_to_pairs' gives correct answer with valid inputs - integers", {
    ans_obtained <- breaks_to_pairs(breaks = c(0L, 1L, 5L, 10L),
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
    ans_obtained <- breaks_to_pairs(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = FALSE,
                                            open_last = TRUE,
                                            include_na = TRUE)
    ans_expected <- list(c(0L, 1L),
                         c(1L, 5L),
                         c(5L, 10L),
                         c(10L, NA),
                         c(NA_integer_, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = TRUE)
    ans_expected <- list(c(0L, 1L),
                         c(1L, 5L),
                         c(5L, 10L),
                         c(NA_integer_, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = FALSE)
    ans_expected <- list(c(0L, 1L),
                         c(1L, 5L),
                         c(5L, 10L))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs(breaks = integer(),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = FALSE)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs(breaks = integer(),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = TRUE)
    ans_expected <- list(c(NA_integer_, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs(breaks = 0L,
                                            open_first = TRUE,
                                            open_last = TRUE,
                                            include_na = FALSE)
    ans_expected <- list(c(NA_integer_, 0L),
                         c(0L, NA_integer_))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'breaks_to_pairs' gives correct answer with valid inputs - dates", {
    ans_obtained <- breaks_to_pairs(breaks = as.Date(c("2020-01-01",
                                                               "2020-05-13",
                                                               "2020-05-14")),
                                            open_first = TRUE,
                                            open_last = TRUE,
                                            include_na = TRUE)
    ans_expected <- list(as.Date(c(NA, "2020-01-01")),
                         as.Date(c("2020-01-01", "2020-05-13")),
                         as.Date(c("2020-05-13", "2020-05-14")),
                         as.Date(c("2020-05-14", NA)),
                         as.Date(c(NA, NA)))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs(breaks = as.Date(character()),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = TRUE)
    ans_expected <- list(as.Date(c(NA, NA)))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- breaks_to_pairs(breaks = as.Date("2020-05-01"),
                                            open_first = TRUE,
                                            open_last = TRUE,
                                            include_na = FALSE)
    ans_expected <- list(as.Date(c(NA, "2020-05-01")),
                         as.Date(c("2020-05-01", NA)))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'breaks_to_pairs' raises expected error with invalid inputs", {
    expect_error(breaks_to_pairs(breaks = integer(),
                                         open_first = TRUE,
                                         open_last = FALSE,
                                         include_na = FALSE),
                 "'breaks' has length 0 but 'open_first' is TRUE")
    expect_error(breaks_to_pairs(breaks = integer(),
                                         open_first = FALSE,
                                         open_last = TRUE,
                                         include_na = FALSE),
                 "'breaks' has length 0 but 'open_last' is TRUE")
    expect_error(breaks_to_pairs(breaks = as.Date("2020-01-01"),
                                         open_first = FALSE,
                                         open_last = FALSE,
                                         include_na = FALSE),
                 "'breaks' has length 1 but 'open_first' and 'open_last' are both FALSE")
})
