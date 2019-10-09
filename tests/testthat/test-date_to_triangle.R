
context("date_to_triangle")

## date_to_triangle_year --------------------------------------------------

test_that("date_to_triangle_year gives correct answers with valid inputs - first_month is Jan", {
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    "2000-02-29"),
                                           dob = "2000-01-01"),
                     factor(c("Lower", "Lower", "Lower", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    "2000-09-29",
                                                    "2000-06-14",
                                                    "2000-06-15",
                                                    "2000-06-16"),
                                           dob = "1999-06-15"),
                     factor(c("Upper", "Upper", "Upper", "Lower", "Upper", "Lower", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    NA,
                                                    "2000-12-29"),
                                           dob = "1999-12-15"),
                     factor(c("Upper", "Upper", "Upper", NA, "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = "2001-02-28",
                                           dob = c("2000-02-29",
                                                   "2000-02-28")),
                     factor(c("Upper", "Upper"), levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = "2001-01-31",
                                           dob = "2000-02-29"),
                     factor("Upper", levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = "2001-01-31",
                                           dob = "1960-02-29",
                                           break_max = 30),
                     factor("Upper", levels = c("Lower", "Upper")))
})

test_that("date_to_triangle_year gives correct answers with valid inputs - first_month is Jul", {
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    "2000-02-29",
                                                    "2000-07-01",
                                                    "2000-12-31"),
                                           dob = "2000-01-01",
                                           first_month = "Jul"),
                     factor(c("Lower", "Lower", "Lower", "Lower", "Upper", "Upper"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    "2000-09-29",
                                                    "2000-06-14",
                                                    "2000-06-15",
                                                    "2000-06-16"),
                                           dob = "1999-06-15",
                                           first_month = "Jul"),
                     factor(c("Upper", "Upper", "Upper", "Upper", "Upper", "Lower", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                    "2000-01-02",
                                                    "2000-03-11",
                                                    NA,
                                                    "2000-12-29",
                                                    "2000-07-01"),
                                           dob = "1999-12-15",
                                           first_month = "Jul"),
                     factor(c("Lower", "Lower", "Lower", NA, "Lower", "Upper"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = "2001-02-28",
                                           dob = c("2000-02-29",
                                                   "2000-02-28"),
                                           first_month = "Jul"),
                     factor(c("Upper", "Upper"), levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = "2001-01-31",
                                           dob = "2000-02-29"),
                     factor("Upper", levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_year(date = "2001-01-31",
                                           dob = "1960-02-29",
                                           first_month = "Jul",
                                           break_max = 30),
                     factor("Upper", levels = c("Lower", "Upper")))
})

test_that("date_to_triangle_year gives correct answers with open age group", {
    expect_identical(date_to_triangle_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                    "2004-12-31",
                                                    "1999-12-31",
                                                    NA),
                                            dob = "1900-01-01",
                                            break_max = 100),
                     factor(c("Upper", "Upper", "Upper", "Lower", NA),
                            levels = c("Lower", "Upper")))
})

test_that("date_to_triangle_year gives correct answers with leap years", {
    expect_identical(date_to_triangle_year(date = c("2001-02-27",
                                                    "2001-02-28",
                                                    "2004-02-27",
                                                    "2004-02-28",
                                                    "2004-02-29"),
                                           dob = "2000-02-29"),
                     factor(c("Upper", "Upper", "Upper", "Upper", "Upper"),
                            levels = c("Lower", "Upper")))
})


## date_to_triangle_multi --------------------------------------------------

test_that("date_to_triangle_multi gives correct answers with valid inputs", {
    expect_identical(date_to_triangle_multi(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2000-01-03"),
                                            dob = "2000-01-01",
                                            width = 5),
                     factor(c("Lower", "Lower", "Lower"), 
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_multi(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01",
                                            break_max = 10),
                     factor(c("Lower", "Upper", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_multi(date = c("2001-01-01",
                                                     "2010-01-01",
                                                     NA),
                                            dob = "2000-07-01",
                                            width = 5),
                     factor(c("Lower", "Upper", NA),
                            levels = c("Lower", "Upper")))
})


## ## date_to_triangle_fert --------------------------------------------------

test_that("date_to_triangle_fert gives correct answers with valid inputs", {
    expect_identical(date_to_triangle_fert(date = c("2015-01-01",
                                                    "2025-01-01",
                                                    "2029-12-31"),
                                           dob = "2000-01-01"),
                     factor(c("Lower", "Lower", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_fert(date = c("2020-01-01",
                                                    "2025-01-01",
                                                    "2029-12-31"),
                                           dob = "2000-02-01"),
                     factor(c("Upper", "Upper", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_fert(date = c("2015-01-01",
                                                    "2025-01-01",
                                                    "2029-12-31",
                                                    NA),
                                           dob = "2000-01-01",
                                           break_min = 10,
                                           break_max = 55),
                     factor(c("Lower", "Lower", "Lower", NA),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_fert(date = c("2015-01-01",
                                                    "2025-01-01",
                                                    "2049-12-31"),
                                           dob = "2000-01-01",
                                           break_min = 20,
                                           break_max = 40,
                                           recode_up = TRUE,
                                           recode_down = TRUE),
                     factor(c("Lower", "Lower", "Lower"),
                            levels = c("Lower", "Upper")))
})

test_that("date_to_triangle_fert throws correct errors with invalid inputs", {
    expect_error(date_to_triangle_fert(date = c("2015-01-01",
                                                 "2025-01-01",
                                                 "2029-12-31"),
                                        dob = "2000-01-01",
                                        width = 3),
                 "difference between 'break_max' \\[50\\] and 'break_min' \\[15\\] not divisible by 'width' \\[3\\]")
    expect_error(date_to_triangle_fert(date = c("2015-01-01",
                                                 "2025-01-01",
                                                 "2029-12-31"),
                                        dob = "2000-01-01",
                                        break_min = 20),
                 paste("'date' of \"2015-01-01\" and 'dob' of \"2000-01-01\" imply age of 15,",
                       "but 'break_min' is 20 and 'recode_up' is FALSE"))
    expect_error(date_to_triangle_fert(date = c("2045-01-01",
                                                 "2025-01-01",
                                                 "2029-12-31"),
                                        dob = "2000-01-01",
                                        break_max = 45),
                 paste("'date' of \"2045-01-01\" and 'dob' of \"2000-01-01\" imply age of 45,",
                       "but 'break_max' is 45 and 'recode_down' is FALSE"))
})


## date_to_triangle_quarter ---------------------------------------------------

test_that("date_to_triangle_quarter gives correct answers with valid inputs", {
    expect_identical(date_to_triangle_quarter(date = c("2000-01-01",
                                                       "2000-01-02",
                                                       "2000-03-11",
                                                       "2000-02-29"),
                                              dob = "2000-01-01"),
                     factor(c("Lower", "Lower", "Lower", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_quarter(date = c("2000-01-01",
                                                       "2000-01-02",
                                                       "2000-03-11",
                                                       "2000-02-29"),
                                              dob = "1999-01-15"),
                     factor(c("Upper", "Upper", "Lower", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_quarter(date = c("2000-01-01",
                                                       "2000-01-02",
                                                       "2000-03-11",
                                                       NA,
                                                       "2000-03-29"),
                                              dob = "1999-12-15"),
                     factor(c("Upper", "Upper", "Upper", NA, "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_quarter(date = "2001-02-28",
                                              dob = "2000-03-31"),
                     factor("Upper", levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_quarter(date = "2001-01-31",
                                              dob = "2000-02-29"),
                     factor("Upper", levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_quarter(date = "2001-01-31",
                                              dob = "1960-02-29",
                                              break_max = 10),
                     factor("Upper", levels = c("Lower", "Upper")))
})


## date_to_triangle_month ---------------------------------------------------

test_that("date_to_triangle_month gives correct answers with valid inputs", {
    expect_identical(date_to_triangle_month(date = c("2000-01-01",
                                                     "2000-01-02",
                                                     "2000-03-11",
                                                     "2000-02-29"),
                                            dob = "2000-01-01"),
                     factor(c("Lower", "Lower", "Lower", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_month(date = c("2000-01-01",
                                                     NA,
                                                     "2000-01-02",
                                                     "2000-03-11",
                                                     "2000-02-29"),
                                            dob = "1999-01-15"),
                     factor(c("Upper", NA, "Upper", "Upper", "Lower"),
                            levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_month(date = "2001-02-28",
                                            dob = "2001-01-31"),
                     factor("Upper", levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_month(date = "2001-01-31",
                                            dob = "2000-02-29"),
                     factor("Lower", levels = c("Lower", "Upper")))
    expect_identical(date_to_triangle_month(date = "2001-01-31",
                                            dob = "1960-02-29",
                                            break_max = 10),
                     factor("Upper", levels = c("Lower", "Upper")))
})





