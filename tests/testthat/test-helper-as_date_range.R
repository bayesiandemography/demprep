
context("helper-as_date_range")

## as_date_range_month_quarter -------------------------------------------------

test_that("'as_date_range_month_quarter' gives correct answer with valid inputs", {
    x <- c("2000 Q2", NA, "<2000 Q1", "2000 Q2")
    expect_identical(as_date_range_month_quarter(x, parse_fun = parse_quarters),
                     factor(c("[2000-04-01, 2000-06-30]",
                              NA,
                              "(-Inf, 1999-12-31]",
                              "[2000-04-01, 2000-06-30]"),
                            levels = c("(-Inf, 1999-12-31]",
                                       "[2000-04-01, 2000-06-30]",
                                       NA),
                            exclude = NULL))
    x <- c("2000 Feb", NA, "<2000 Jan", "2000 Feb")
    expect_identical(as_date_range_month_quarter(x, parse_fun = parse_months),
                     factor(c("[2000-02-01, 2000-02-29]",
                              NA,
                              "(-Inf, 1999-12-31]",
                              "[2000-02-01, 2000-02-29]"),
                            levels = c("(-Inf, 1999-12-31]",
                                       "[2000-02-01, 2000-02-29]",
                                       NA),
                            exclude = NULL))
    expect_identical(as_date_range_month_quarter(character(), parse_fun = parse_quarters),
                     factor())
    expect_identical(as_date_range_month_quarter(NA, parse_fun = parse_quarters),
                     factor(NA, exclude = NULL ))
})

test_that("'as_date_range_month_quarter' throws correct error with invalid inputs", {
    expect_error(as_date_range_month_quarter("2000 Mar+", parse_fun = parse_months),
                 "'x' has interval \\[\"2000 Mar\\+\"\\] that is open on the right")
})


## order_low_up ---------------------------------------------------------------

test_that("'order_low_up' gives correct answer with valid inputs", {
    expect_identical(order_low_up(low = c(0L, NA, 5L, NA, 15L, 10L),
                                  up = c(5L, 0L, 10L, NA, NA, 15L)),
                     c(2L, 1L, 3L, 6L, 5L, 4L))
    expect_identical(order_low_up(low = c(0L, NA, NA),
                                  up = c(5L, 10L, 5L)),
                     c(3L, 2L, 1L))
    expect_identical(order_low_up(low = integer(),
                                  up = integer()),
                     integer())
    expect_identical(order_low_up(low = c(NA, NA, NA),
                                  up = c(NA, 0L, NA)),
                     c(2L, 1L, 3L))
})
