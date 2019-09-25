
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

## ## date_to_period_quarter ---------------------------------------------------

## test_that("date_to_period_quarter gives correct answers with valid inputs", {
##     expect_identical(date_to_period_quarter(date = c("2000-01-01",
##                                                         "2000-05-11",
##                                                         "2001-02-28"),
##                                                dob = "2000-01-01"),
##                      factor(c("0q", "1q", "4q"),
##                             levels = c(paste0(0:399, "q"), "400q+")))
##     expect_identical(date_to_period_quarter(date = c("2000-01-01",
##                                                         "2000-05-11",
##                                                         "2001-04-28"),
##                                                dob = "2000-01-01",
##                                                break_max = 5L),
##                      factor(c("0q", "1q", "5q+"),
##                             levels = c(paste0(0:4, "q"), "5q+")))
##     expect_identical(date_to_period_quarter(date = c("2000-01-01",
##                                                         "2000-05-11",
##                                                         NA,
##                                                         "2001-04-28"),
##                                                dob = "2000-01-01",
##                                                break_max = 6,
##                                                open_right = FALSE),
##                      factor(c("0q", "1q", NA, "5q"),
##                             levels = c("0q", "1q", "2q", "3q", "4q", "5q", NA),
##                             exclude = NULL))
## })

## ## date_to_period_month ---------------------------------------------------

## test_that("date_to_period_month gives correct answers with valid inputs", {
##     expect_identical(date_to_period_month(date = c("2000-01-01",
##                                                       "2000-03-11",
##                                                       "2000-02-29"),
##                                              dob = "2000-01-01"),
##                      factor(c("0m", "2m", "1m"),
##                             levels = c(paste0(0:1199, "m"), "1200m+")))
##     expect_identical(date_to_period_month(date = c("2000-01-01",
##                                                       "2000-03-11",
##                                                       "2000-02-29"),
##                                              dob = "2000-01-01",
##                                              break_max = 3),
##                      factor(c("0m", "2m", "1m"),
##                             levels = c("0m", "1m", "2m", "3m+")))
##     expect_identical(date_to_period_month(date = c("2000-01-01",
##                                                       "2000-03-11",
##                                                       NA,
##                                                       "2000-02-29"),
##                                              dob = "2000-01-01",
##                                              break_max = 3,
##                                              open_right = FALSE),
##                      factor(c("0m", "2m", NA, "1m"),
##                             levels = c("0m", "1m", "2m", NA),
##                             exclude = NULL))
## })





