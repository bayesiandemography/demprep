
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


## parse_quarters -------------------------------------------------------------

test_that("'parse_quarters' gives correct answer with valid inputs", {
    expect_identical(parse_quarters(c("2020 Q1", "2020 Q3", NA, "2020 Q1", "<2020 Q2")),
                     list(low = as.Date(c("2020-01-01", "2020-07-01", NA, "2020-01-01", NA)),
                          up = as.Date(c("2020-04-01", "2020-10-01", NA, "2020-04-01", "2020-04-01")),
                          is_open_first = c(FALSE, FALSE, FALSE, FALSE, TRUE),
                          is_open_last = c(FALSE, FALSE, FALSE, FALSE, FALSE),
                          break_min = as.Date("2020-04-01"),
                          break_max = as.Date("2020-10-01")))
    expect_identical(parse_quarters(c("2010 Q4", "<2000 Q3")),
                     list(low = as.Date(c("2010-10-01", NA)),
                          up = as.Date(c("2011-01-01", "2000-07-01")),
                          is_open_first = c(FALSE, TRUE),
                          is_open_last = c(FALSE, FALSE),
                          break_min = as.Date("2000-07-01"),
                          break_max = as.Date("2011-01-01")))
})


## parse_months ---------------------------------------------------------------

test_that("'parse_months' gives correct answer with valid inputs", {
    expect_identical(parse_months(c("2020 Jan", "2020 Aug", NA, "2020 Feb", "<2020 Mar")),
                     list(low = as.Date(c("2020-01-01", "2020-08-01", NA, "2020-02-01", NA)),
                          up = as.Date(c("2020-02-01", "2020-09-01", NA, "2020-03-01", "2020-03-01")),
                          is_open_first = c(FALSE, FALSE, FALSE, FALSE, TRUE),
                          is_open_last = c(FALSE, FALSE, FALSE, FALSE, FALSE),
                          break_min = as.Date("2020-03-01"),
                          break_max = as.Date("2020-09-01")))
    expect_identical(parse_months(c("2010 Nov", "<2000 Jul")),
                     list(low = as.Date(c("2010-11-01", NA)),
                          up = as.Date(c("2010-12-01", "2000-07-01")),
                          is_open_first = c(FALSE, TRUE),
                          is_open_last = c(FALSE, FALSE),
                          break_min = as.Date("2000-07-01"),
                          break_max = as.Date("2010-12-01")))
})
