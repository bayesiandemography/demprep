
## date_to_triangle_year --------------------------------------------------

test_that("date_to_triangle_year gives correct answers with valid inputs - month_start is Jan", {
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    "2000-02-29"),
                                           dob = "2000-01-01"),
                     c("Lower", "Lower", "Lower", "Lower"))
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    "2000-09-29",
                                                    "2000-06-14",
                                                    "2000-06-15",
                                                    "2000-06-16"),
                                           dob = "1999-06-15"),
                     c("Upper", "Upper", "Upper", "Lower", "Upper", "Lower", "Lower"))
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    NA,
                                                    "2000-12-29"),
                                           dob = "1999-12-15"),
                     c("Upper", "Upper", "Upper", NA, "Lower"))
    expect_identical(date_to_triangle_year(date = NA,
                                           dob = "1999-12-15"),
                     NA_character_)
    expect_identical(date_to_triangle_year(date = "2001-02-28",
                                           dob = c("2000-02-29",
                                                   "2000-02-28")),
                     c("Upper", "Lower"))
    expect_identical(date_to_triangle_year(date = "2001-01-31",
                                           dob = "2000-02-29"),
                     "Upper")
})

test_that("date_to_triangle_year gives correct answers with valid inputs - month_start is Jul", {
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    "2000-02-29",
                                                    "2000-07-01",
                                                    "2000-12-31"),
                                           dob = "2000-01-01",
                                           month_start = "Jul"),
                     c("Lower", "Lower", "Lower", "Lower", "Upper", "Upper"))
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    "2000-09-29",
                                                    "2000-06-14",
                                                    "2000-06-15",
                                                    "2000-06-16"),
                                           dob = "1999-06-15",
                                           month_start = "Jul"),
                     c("Upper", "Upper", "Upper", "Upper", "Upper", "Lower", "Lower"))
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    NA,
                                                    "2000-12-29",
                                                    "2000-07-01"),
                                           dob = "1999-12-15",
                                           month_start = "Jul"),
                     c("Lower", "Lower", "Lower", NA, "Lower", "Upper"))
    expect_identical(date_to_triangle_year(date = "2001-02-28",
                                           dob = c("2000-02-29",
                                                   "2000-02-28"),
                                           month_start = "Jul"),
                     c("Upper", "Lower"))
    expect_identical(date_to_triangle_year(date = "2001-01-31",
                                           dob = "2000-02-29"),
                     "Upper")
})

test_that("date_to_triangle_year gives correct answers with leap years", {
    expect_identical(date_to_triangle_year(date = c("2001-02-27",
                                                    "2001-02-28",
                                                    "2004-02-27",
                                                    "2004-02-28",
                                                    "2004-02-29"),
                                           dob = "2000-02-29"),
                     c("Upper", "Upper", "Upper", "Upper", "Lower"))
})

test_that("date_to_triangle_year gives correct answers with all NA", {
    expect_identical(date_to_triangle_year(date = c("2001-01-01", NA),
                                           dob = c(NA, "2000-01-01")),
                     rep(NA_character_, 2))
})


## date_to_triangle_quarter ---------------------------------------------------

test_that("date_to_triangle_quarter gives correct answers with valid inputs", {
    expect_identical(date_to_triangle_quarter(date = c("2000-01-01",
                                                       "2000-01-02",
                                                       "2000-03-11",
                                                       "2000-02-29"),
                                              dob = "2000-01-01"),
                     c("Lower", "Lower", "Lower", "Lower"))
    expect_identical(date_to_triangle_quarter(date = c("2000-01-01",
                                                       "2000-01-02",
                                                       "2000-03-11",
                                                       "2000-02-29"),
                                              dob = "1999-01-15"),
                     c("Upper", "Upper", "Lower", "Lower"))
    expect_identical(date_to_triangle_quarter(date = c("2000-01-01",
                                                       "2000-01-02",
                                                       "2000-03-11",
                                                       NA,
                                                       "2000-03-29"),
                                              dob = "1999-12-15"),
                     c("Upper", "Upper", "Upper", NA, "Lower"))
    expect_identical(date_to_triangle_quarter(date = "2001-02-28",
                                              dob = "2000-03-31"),
                     "Upper")
    expect_identical(date_to_triangle_quarter(date = "2001-01-31",
                                              dob = "2000-02-29"),
                     "Upper")
})


## date_to_triangle_month ---------------------------------------------------

test_that("date_to_triangle_month gives correct answers with valid inputs", {
    expect_identical(date_to_triangle_month(date = c("2000-01-01",
                                                     "2000-01-02",
                                                     "2000-03-11",
                                                     "2000-02-29"),
                                            dob = "2000-01-01"),
                     c("Lower", "Lower", "Lower", "Lower"))
    expect_identical(date_to_triangle_month(date = c("2000-01-01",
                                                     NA,
                                                     "2000-01-02",
                                                     "2000-03-11",
                                                     "2000-02-29"),
                                            dob = "1999-01-15"),
                     c("Upper", NA, "Upper", "Upper", "Lower"))
    expect_identical(date_to_triangle_month(date = "2001-02-28",
                                            dob = "2001-01-31"),
                     "Upper")
    expect_identical(date_to_triangle_month(date = "2001-01-31",
                                            dob = "2000-02-29"),
                     "Lower")
    expect_identical(date_to_triangle_month(date = c("2001-01-01", NA),
                                            dob = c(NA, "2000-01-01")),
                     rep(NA_character_, 2))
})
