
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


## as_date_range_multi --------------------------------------------------------

test_that("'as_date_range_multi' gives correct answer with valid inputs", {
    x <- c("2000-2005", NA, "<2000", "2000-2005")
    expect_identical(as_date_range_multi(x),
                     factor(c("[2000-01-01, 2004-12-31]",
                              NA,
                              "(-Inf, 1999-12-31]",
                              "[2000-01-01, 2004-12-31]"),
                            levels = c("(-Inf, 1999-12-31]",
                                       "[2000-01-01, 2004-12-31]",
                                       NA),
                            exclude = NULL))
    x <- c("2000-2005", NA, "<2000", "2000-2005")
    expect_identical(as_date_range_multi(x, month_start = "Nov"),
                     factor(c("[2000-11-01, 2005-10-31]",
                              NA,
                              "(-Inf, 2000-10-31]",
                              "[2000-11-01, 2005-10-31]"),
                            levels = c("(-Inf, 2000-10-31]",
                                       "[2000-11-01, 2005-10-31]",
                                       NA),
                            exclude = NULL))
    expect_identical(as_date_range_multi(character(), month_start = "Nov"),
                     factor())
    expect_identical(as_date_range_multi(NA, month_start = "Nov"),
                     factor(NA, exclude = NULL ))
})

test_that("'as_date_range_custom_multi' throws correct error with invalid inputs", {
    expect_error(as_date_range_multi("2000+", month_start = "Jan"),
                 "'x' has interval \\[\"2000\\+\"\\] that is open on the right")
    x <- c("2000-2005", NA, "<2000", "2005-2011")
    expect_error(as_date_range_multi(x),
                 "intervals \"2000-2005\" and \"2005-2011\" in 'x' have different widths")
    x <- c("2000-2005", NA, "<2000", "2006-2011")
    expect_error(as_date_range_multi(x),
                 "gaps between intervals \"<2000\" and \"2006-2011\" in 'x' not divisible by width of intervals \\[5\\]")
})


## as_date_range_custom -------------------------------------------------------

test_that("'as_date_range_custom' gives correct answer with valid inputs", {
    x <- c("2000-2005", NA, "<2000", "2000-2005")
    expect_identical(as_date_range_custom(x),
                     factor(c("[2000-01-01, 2004-12-31]",
                              NA,
                              "(-Inf, 1999-12-31]",
                              "[2000-01-01, 2004-12-31]"),
                            levels = c("(-Inf, 1999-12-31]",
                                       "[2000-01-01, 2004-12-31]",
                                       NA),
                            exclude = NULL))
    x <- c("2000-2006", NA, "<2000", "2000-2006")
    expect_identical(as_date_range_custom(x, month_start = "November"),
                     factor(c("[2000-11-01, 2006-10-31]",
                              NA,
                              "(-Inf, 2000-10-31]",
                              "[2000-11-01, 2006-10-31]"),
                            levels = c("(-Inf, 2000-10-31]",
                                       "[2000-11-01, 2006-10-31]",
                                       NA),
                            exclude = NULL))
    expect_identical(as_date_range_custom(character(), month_start = "Nov"),
                     factor())
    expect_identical(as_date_range_custom(NA, month_start = "Nov"),
                     factor(NA, exclude = NULL ))
})

test_that("'as_date_range_custom' throws correct error with invalid inputs", {
    expect_error(as_date_range_custom("2000+", month_start = "Jan"),
                 "'x' has interval \\[\"2000\\+\"\\] that is open on the right")
})


## as_date_range_quarter ------------------------------------------------------

test_that("'as_date_range_quarter' gives correct answer with valid inputs", {
    x <- c("2000 Q2", NA, "<2000 Q1", "2000 Q2")
    expect_identical(as_date_range_quarter(x),
                     factor(c("[2000-04-01, 2000-06-30]",
                              NA,
                              "(-Inf, 1999-12-31]",
                              "[2000-04-01, 2000-06-30]"),
                            levels = c("(-Inf, 1999-12-31]",
                                       "[2000-04-01, 2000-06-30]",
                                       NA),
                            exclude = NULL))
    expect_identical(as_date_range_quarter(character()),
                     factor())
    expect_identical(as_date_range_quarter(NA),
                     factor(NA, exclude = NULL))
})

test_that("'as_date_range_quarter' throws correct error with invalid inputs", {
    expect_error(as_date_range_quarter("2000 Q3+"),
                 "'x' has interval \\[\"2000 Q3\\+\"\\] that is open on the right")
})


## as_date_range_quarter ------------------------------------------------------

test_that("'as_date_range_month' gives correct answer with valid inputs", {
    x <- c("2000 Feb", NA, "<2000 Jan", "2000 Feb")
    expect_identical(as_date_range_month(x),
                     factor(c("[2000-02-01, 2000-02-29]",
                              NA,
                              "(-Inf, 1999-12-31]",
                              "[2000-02-01, 2000-02-29]"),
                            levels = c("(-Inf, 1999-12-31]",
                                       "[2000-02-01, 2000-02-29]",
                                       NA),
                            exclude = NULL))
    expect_identical(as_date_range_month(character()),
                     factor())
    expect_identical(as_date_range_month(NA),
                     factor(NA, exclude = NULL))
})

test_that("'as_date_range_month' throws correct error with invalid inputs", {
    expect_error(as_date_range_month("2000 Mar+"),
                 "'x' has interval \\[\"2000 Mar\\+\"\\] that is open on the right")
})
