
## parse_integers -------------------------------------------------------------

test_that("'parse_integers' gives correct answer with valid inputs", {
    expect_identical(parse_integers(c("10", "<5", NA, "20+", "1"),
                                    name = "x"),
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

test_that("'parse_integers' throws correct error with invalid inputs", {
    expect_error(parse_integers(c("10", "wrong", NA, "20", "1"),
                                name = "x"),
                 "'x' has invalid label \\[\"wrong\"\\]")
})


## parse_integers_intervals ---------------------------------------------------

test_that("'parse_integers_intervals' gives correct answer with valid inputs - 'label_open_multi' not needed", {
    expect_identical(parse_integers_intervals(c("10-11", "14", "<5", NA, "20+", "1-5"),
                                              name = "x",
                                              month_start = "Jan",
                                              label_year_start = TRUE,
                                              label_open_multi = NULL),
                     list(low = c(10L, 14L, NA, NA, 20L, 1L),
                          up = c(11L, 15L, 5L, NA, NA, 5L),
                          is_open_first = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                          break_min = 5L,
                          break_max = 20L))
})


test_that("'parse_integers_intervals' gives correct answer with valid inputs - 'label_open_multi' is TRUE", {
    expect_identical(parse_integers_intervals(c("10-11", "14", "<5", NA, "20+", "1-5"),
                                              name = "x",
                                              month_start = "Feb",
                                              label_year_start = FALSE,
                                              label_open_multi = FALSE),
                     list(low = c(10L, 13L, NA, NA, 20L, 1L),
                          up = c(11L, 14L, 4L, NA, NA, 5L),
                          is_open_first = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                          break_min = 4L,
                          break_max = 20L))
})

test_that("'parse_integers_intervals' gives correct answer with valid inputs - 'label_open_multi' is TRUE", {
    expect_identical(parse_integers_intervals(c("10-11", "14", "<5", NA, "20+", "1-5"),
                                              name = "x",
                                              month_start = "Feb",
                                              label_year_start = FALSE,
                                              label_open_multi = TRUE),
                     list(low = c(10L, 13L, NA, NA, 20L, 1L),
                          up = c(11L, 14L, 5L, NA, NA, 5L),
                          is_open_first = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                          break_min = 5L,
                          break_max = 20L))
})

test_that("'parse_integers_intervals' throws correct error with invalid inputs", {
    expect_error(parse_integers_intervals(c("10-14", "<5", NA, "20-20", "1-5"),
                                          name = "x",
                                          month_start = "Jan",
                                          label_year_start = TRUE,
                                          label_open_multi = TRUE),
                 "'x' has label with upper limit less than or equal to lower limit \\[\"20-20\"\\]")
    expect_error(parse_integers_intervals(c("10-14", "5-4", NA, "20-21", "1-5"),
                                          name = "x",
                                          month_start = "Jan",
                                          label_year_start = TRUE,
                                          label_open_multi = TRUE),
                 "'x' has label with upper limit less than or equal to lower limit \\[\"5-4\"\\]")
    expect_error(parse_integers_intervals(c("10-14", "<0", NA, "20-21", "1"),
                                          name = "x",
                                          month_start = "Mar",
                                          label_year_start = FALSE,
                                          label_open_multi = NULL),
                 "value for 'label_open_multi' needed to interpret ambiguous label \"<0\"")
})



## parse_intervals ---------------------------------------------------

test_that("'parse_intervals' gives correct answer with valid inputs", {
    expect_identical(parse_intervals(c("10-11", "<5", NA, "20+", "1-5"),
                                     name = "x"),
                     list(low = c(10L, NA, NA, 20L, 1L),
                          up = c(11L, 5L, NA, NA, 5L),
                          is_open_first = c(FALSE, TRUE, FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE, TRUE, FALSE),
                          break_min = 5L,
                          break_max = 20L))
})

test_that("'parse_intervals' throws correct error with invalid inputs", {
    expect_error(parse_intervals(c("10", NA, "20-20", "1-5"),
                                 name = "x"),
                 "'x' has invalid label \\[\"10\"\\]")
    expect_error(parse_intervals(c("10-14", "<5", NA, "20-20", "1-5"),
                                 name = "x"),
                 "'x' has label with upper limit less than or equal to lower limit \\[\"20-20\"\\]")
    expect_error(parse_intervals(c("10-14", "5-4", NA, "20-21", "1-5"),
                                 name = "x"),
                 "'x' has label with upper limit less than or equal to lower limit \\[\"5-4\"\\]")
})


## parse_months ---------------------------------------------------------------

test_that("'parse_months' gives correct answer with valid inputs", {
    expect_identical(parse_months(c("2020 Jan", "2020 Aug", NA, "2020 Feb", "<2020 Mar"),
                                  name = "x"),
                     list(low = as.Date(c("2020-01-01", "2020-08-01", NA, "2020-02-01", NA)),
                          up = as.Date(c("2020-02-01", "2020-09-01", NA, "2020-03-01", "2020-03-01")),
                          is_open_first = c(FALSE, FALSE, FALSE, FALSE, TRUE),
                          is_open_last = c(FALSE, FALSE, FALSE, FALSE, FALSE),
                          break_min = as.Date("2020-03-01"),
                          break_max = as.Date("2020-09-01")))
    expect_identical(parse_months(c("2010 Nov", "<2000 Jul", "2010 Oct+"),
                                  name = "x"),
                     list(low = as.Date(c("2010-11-01", NA, "2010-10-01")),
                          up = as.Date(c("2010-12-01", "2000-07-01", NA)),
                          is_open_first = c(FALSE, TRUE, FALSE),
                          is_open_last = c(FALSE, FALSE, TRUE),
                          break_min = as.Date("2000-07-01"),
                          break_max = as.Date("2010-10-01")))
})

test_that("'parse_integers' throws correct error with invalid inputs", {
    expect_error(parse_months(c("2020 January", NA, "2000 Mar"),
                              name = "x"),
                 "'x' has invalid label \\[\"2020 January\"\\]")
})


## parse_quantities -----------------------------------------------------------

test_that("'parse_quantities' gives correct answer with valid inputs", {
    expect_identical(parse_quantities(c("10", "<5", NA, "20+", "1-5", "10-10"),
                                      name = "x"),
                     list(low = c(10L, NA, NA, 20L, 1L, 10L),
                          up = c(11L, 5L, NA, NA, 6L, 11L),
                          is_open_first = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                          break_min = 5L,
                          break_max = 20L))
    expect_identical(parse_quantities(c("10", "5-10", "1"),
                                      name = "x"),
                     list(low = c(10L, 5L, 1L),
                          up = c(11L, 11L, 2L),
                          is_open_first = c(FALSE, FALSE, FALSE),
                          is_open_last = c(FALSE, FALSE, FALSE),
                          break_min = 1L,
                          break_max = 11L))
})

test_that("'parse_quantities' throws correct error with invalid inputs", {
    expect_error(parse_quantities(c("0-4", NA, "5-4"),
                                name = "x"),
                 "'x' has label with upper limit less than lower limit \\[\"5-4\"\\]")
    expect_error(parse_quantities(c("0-4", NA, "5-3"),
                                name = "x"),
                 "'x' has label with upper limit less than lower limit \\[\"5-3\"\\]")
    expect_error(parse_quantities(c("0-4", NA, "5--9"),
                                name = "x"),
                 "'x' has invalid label \\[\"5--9\"\\]")
})


## parse_quarters -------------------------------------------------------------

test_that("'parse_quarters' gives correct answer with valid inputs", {
    expect_identical(parse_quarters(c("2020 Q1", "2020 Q3", NA, "2020 Q1", "<2020 Q2"),
                                    name = "x"),
                     list(low = as.Date(c("2020-01-01", "2020-07-01", NA, "2020-01-01", NA)),
                          up = as.Date(c("2020-04-01", "2020-10-01", NA, "2020-04-01", "2020-04-01")),
                          is_open_first = c(FALSE, FALSE, FALSE, FALSE, TRUE),
                          is_open_last = c(FALSE, FALSE, FALSE, FALSE, FALSE),
                          break_min = as.Date("2020-04-01"),
                          break_max = as.Date("2020-10-01")))
    expect_identical(parse_quarters(c("2010 Q4", "<2000 Q3", "2010 Q3+"),
                                    name = "x"),
                     list(low = as.Date(c("2010-10-01", NA, "2010-07-01")),
                          up = as.Date(c("2011-01-01", "2000-07-01", NA)),
                          is_open_first = c(FALSE, TRUE, FALSE),
                          is_open_last = c(FALSE, FALSE, TRUE),
                          break_min = as.Date("2000-07-01"),
                          break_max = as.Date("2010-07-01")))
})

test_that("'parse_integers' throws correct error with invalid inputs", {
    expect_error(parse_quarters(c("2020 Q5", NA, "2000 Q1"),
                                name = "x"),
                 "'x' has invalid label \\[\"2020 Q5\"\\]")
})



