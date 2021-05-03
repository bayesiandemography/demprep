
context("helper-parse")

## parse_integers -------------------------------------------------------------

test_that("'parse_integers' gives correct answer with valid inputs", {
    expect_identical(parse_integers(c("10", "<5", NA, "20+", "1")),
                     list(low = c(10L, NA, NA, 20L, 1L),
                          up = c(11L, 5L, NA, NA, 2L),
                          is_open_first = c(FALSE, TRUE, FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE, TRUE, FALSE),
                          break_min = 5L,
                          break_max = 20L))
    expect_identical(parse_integers(c("10", "5", "1")),
                     list(low = c(10L, 5L, 1L),
                          up = c(11L, 6L, 2L),
                          is_open_first = c(FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE),
                          break_min = 1L,
                          break_max = 11L))
})


## parse_integers_intervals ---------------------------------------------------

test_that("'parse_integers_intervals' gives correct answer with valid inputs", {
    expect_identical(parse_integers_intervals(c("10", "<5", NA, "20+", "1-5"),
                                              month_start = "Jan",
                                              label_year_start = TRUE),
                     list(low = c(10L, NA, NA, 20L, 1L),
                          up = c(11L, 5L, NA, NA, 5L),
                          is_open_first = c(FALSE, TRUE, FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE, TRUE, FALSE),
                          break_min = 5L,
                          break_max = 20L))
    expect_identical(parse_integers_intervals(c("10", "5-10", "1"),
                                              month_start = "Jul",
                                              label_year_start = FALSE),
                     list(low = c(9L, 5L, 0L),
                          up = c(10L, 10L, 1L),
                          is_open_first = c(FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE),
                          break_min = 0L,
                          break_max = 10L))
})


## parse_quantities -----------------------------------------------------------

test_that("'parse_quantities' gives correct answer with valid inputs", {
    expect_identical(parse_quantities(c("10", "<5", NA, "20+", "1-5")),
                     list(low = c(10L, NA, NA, 20L, 1L),
                          up = c(11L, 5L, NA, NA, 6L),
                          is_open_first = c(FALSE, TRUE, FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE, TRUE, FALSE),
                          break_min = 5L,
                          break_max = 20L))
    expect_identical(parse_quantities(c("10", "5-10", "1")),
                     list(low = c(10L, 5L, 1L),
                          up = c(11L, 11L, 2L),
                          is_open_first = c(FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE),
                          break_min = 1L,
                          break_max = 11L))
})
