
context("date_to_age")

## as_date_range_year ---------------------------------------------------------

test_that("as_date_range_year gives correct answers with default values for 'month_start', 'label_year_start'", {
    x <- c("<2001", "2001", "2002", "2003", NA)
    expect_identical(as_date_range_year(x),
                     factor(c("(-Inf, 2000-12-31]",
                              "[2001-01-01, 2001-12-31]",
                              "[2002-01-01, 2002-12-31]",
                              "[2003-01-01, 2003-12-31]",
                              NA),
                            exclude = NULL))
    x <- c("<2001", "2003", NA, "2001", "2002")
    expect_identical(as_date_range_year(x),
                     factor(c("(-Inf, 2000-12-31]",
                              "[2003-01-01, 2003-12-31]",
                              NA,
                              "[2001-01-01, 2001-12-31]",
                              "[2002-01-01, 2002-12-31]"),
                            levels = c("(-Inf, 2000-12-31]",
                                       "[2001-01-01, 2001-12-31]",
                                       "[2002-01-01, 2002-12-31]",
                                       "[2003-01-01, 2003-12-31]",
                                       NA),
                            exclude = NULL))
})

test_that("as_date_range_year gives correct answers with non-default values for 'month_start', 'label_year_start'", {
    x <- c("<2001", "2001", "2002", "2003", NA)
    expect_identical(as_date_range_year(x,
                                        month_start = "Jul"),
                     factor(c("(-Inf, 2001-06-30]",
                              "[2001-07-01, 2002-06-30]",
                              "[2002-07-01, 2003-06-30]",
                              "[2003-07-01, 2004-06-30]",
                              NA),
                            exclude = NULL))
    x <- c("<2001", "2001", "2002", "2003", NA)
    expect_identical(as_date_range_year(x,
                                        month_start = "Jul",
                                        label_year_start = FALSE),
                     factor(c("(-Inf, 2000-06-30]",
                              "[2000-07-01, 2001-06-30]",
                              "[2001-07-01, 2002-06-30]",
                              "[2002-07-01, 2003-06-30]",
                              NA),
                            exclude = NULL))
})

test_that("as_date_range_year gives correct answers with empty 'x'", {
    x <- character()
    expect_identical(as_date_range_year(x),
                     factor())
    x <- c(NA, NA)
    expect_identical(as_date_range_year(x,
                                        month_start = "Jul",
                                        label_year_start = FALSE),
                     factor(c(NA, NA),
                            exclude = NULL))
})



test_that("as_date_range_years throws correct errors with invalid inputs", {
    x <- c("2000", "2001+")
    expect_error(as_date_range_year(x),
                 "'x' has interval \\[\"2001\\+\"\\] that is open on the right")
})

