
context("date_to_period")

## date_to_period_year --------------------------------------------------

test_that("date_to_period_year gives correct answers with valid inputs", {
    expect_identical(date_to_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         first_month = "Jan"),
                     factor(c("2000", "2010", "2004"),
                            levels = 2000:2010))
    expect_identical(date_to_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         first_month = "Jan"),
                     factor(c("2000", "2010", "2004"),
                            levels = 2000:2010))
    expect_identical(date_to_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         first_month = "Jan",
                                         as_factor = FALSE),
                     c("2000", "2010", "2004"))
    expect_identical(date_to_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         year_to = FALSE,
                                         first_month = "Apr"),
                     factor(c("1999", "2009", "2004"),
                            levels = 1999:2009))
})


## date_to_period_multi --------------------------------------------------

test_that("date_to_period_multi gives correct answers with valid inputs", {
    expect_identical(date_to_period_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31")),
                     factor(c("2000-2005", "2010-2015", "2000-2005"),
                            levels = c("2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_period_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   NA)),
                     factor(c("2000-2005", "2010-2015", NA),
                            levels = c("2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    expect_identical(date_to_period_multi(date = character()),
                     factor(character(),
                            levels = "2000-2005"))
})


## date_to_period_quarter ---------------------------------------------------

test_that("date_to_period_quarter gives correct answers with valid inputs", {
    expect_identical(date_to_period_quarter(date = c("2000-01-01",
                                                     "2000-05-11",
                                                     NA,
                                                     "2001-02-28")),
                     factor(c("2000 Q1", "2000 Q2", NA,  "2001 Q1"),
                            levels = c("2000 Q1", "2000 Q2", "2000 Q3", "2000 Q4",
                                       "2001 Q1", NA),
                            exclude = NULL))
    expect_identical(date_to_period_quarter(date = "2000-01-01"),
                     factor("2000 Q1", levels = "2000 Q1"))
    expect_identical(date_to_period_quarter(date = "2000-01-01",
                                            as_factor = FALSE),
                     "2000 Q1")
})


## date_to_period_month ---------------------------------------------------

test_that("date_to_period_month gives correct answers with valid inputs", {
    expect_identical(date_to_period_month(date = c("2000-01-01",
                                                   "2000-05-11",
                                                   NA,
                                                   "2001-02-28")),
                     factor(c("2000 Jan", "2000 May", NA,  "2001 Feb"),
                            levels = c(paste("2000", month.abb),
                                       "2001 Jan", "2001 Feb",
                                       NA),
                            exclude = NULL))
    expect_identical(date_to_period_month(date = "2000-01-01"),
                     factor("2000 Jan", levels = "2000 Jan"))
    expect_identical(date_to_period_month(date = "2000-01-01",
                                          as_factor = FALSE),
                     "2000 Jan")
})





