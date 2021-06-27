
## date_to_age_year --------------------------------------------------

test_that("date_to_age_year gives correct answers with valid inputs", {
    expect_identical(date_to_age_year(date = c("2000-01-01",
                                               "2010-01-01",
                                               "2004-12-31"),
                                      dob = "2000-01-01"),
                     c(0L, 10L, 4L))
    expect_identical(date_to_age_year(date = c("2000-01-01",
                                               "2010-01-01",
                                               "2004-12-31",
                                               NA),
                                      dob = "2000-01-01"),
                     c(0L, 10L, 4L, NA))
})

test_that("date_to_age_year gives correct answers when 'date' and/or 'dob' has NA", {
    expect_identical(date_to_age_year(date = c("2000-01-01",
                                               "2010-01-01",
                                               NA),
                                      dob = "2000-01-01"),
                     c(0L, 10L, NA))
    expect_identical(date_to_age_year(date = c("2000-01-01",
                                               "2010-01-01",
                                               NA),
                                      dob = c(NA,
                                              NA,
                                              "2000-01-01")),
                     rep(NA_integer_, 3))
})

test_that("date_to_age_year gives correct answers with leap years", {
    expect_identical(date_to_age_year(date = c("2001-02-27",
                                               "2001-02-28",
                                               "2001-03-01",
                                               "2004-02-27",
                                               "2004-02-28",
                                               "2004-02-29",
                                               "2004-03-01"),
                                      dob = "2000-02-29"),
                     c(0L, 0L, 1L, 3L, 3L, 4L, 4L))
})


## date_to_age_quarter ---------------------------------------------------

test_that("date_to_age_quarter gives correct answers with valid inputs", {
    expect_identical(date_to_age_quarter(date = c("2000-01-01",
                                                  "2000-05-11",
                                                  "2001-02-28"),
                                         dob = "2000-01-01"),
                     c(0L, 1L, 4L))
    expect_identical(date_to_age_quarter(date = c("2000-03-11",
                                                  NA),
                                         dob = c(NA,
                                                 "2000-01-01")),
                     c(NA_integer_, NA_integer_))
})


## date_to_age_month ---------------------------------------------------

test_that("date_to_age_month gives correct answers with valid inputs", {
    expect_identical(date_to_age_month(date = c("2000-01-01",
                                                "2000-03-11",
                                                "2000-02-29"),
                                       dob = "2000-01-01"),
                     c(0L, 2L, 1L))
    expect_identical(date_to_age_month(date = c("2000-03-11",
                                                NA),
                                       dob = c(NA,
                                               "2000-01-01")),
                     c(NA_integer_, NA_integer_))
})
