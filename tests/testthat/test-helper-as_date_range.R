
context("helper-as_date_range")

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
