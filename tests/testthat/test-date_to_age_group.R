
context("date_to_age_group")

## date_to_age_group_year --------------------------------------------------

test_that("date_to_age_group_year gives correct answers with alternative finite values for 'age_max'", {
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01"),
                     factor(c(0, 10, 4), levels = c(0:99, "100+")))
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01",
                                            age_max = 10),
                     factor(c(0, "10+", 4), levels = c(0:9, "10+")))
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01",
                                            age_max = 2),
                     factor(c(0, "2+", "2+"), levels = c(0:1, "2+")))
})

test_that("date_to_age_group_year gives correct answers with infinite values for 'age_max'", {
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01",
                                            age_max = Inf),
                     factor(c(0, "10+", 4), levels = c(0:9, "10+")))
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01",
                                            age_max = Inf,
                                            open_right = FALSE),
                     factor(c(0, 10, 4), levels = 0:10))
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01",
                                            age_max = Inf,
                                            open_right = FALSE,
                                            as_factor = FALSE),
                     c("0", "10", "4"))
})


test_that("date_to_age_group_year gives correct answers when 'open_right' is FALSE", {
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01",
                                            open_right = FALSE),
                     factor(c(0, 10, 4), levels = 0:99))
})

test_that("date_to_age_group_year gives correct answers when 'as_factor' is FALSE", {
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            dob = "2000-01-01",
                                            age_max = 2,
                                            as_factor = FALSE),
                     c(0, "2+", "2+"))
})

test_that("date_to_age_group_year gives correct answers when 'date' and/or 'dob' has NA", {
    expect_identical(date_to_age_group_year(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     NA),
                                            dob = "2000-01-01"),
                     factor(c(0, "10", NA), levels = c(0:99, "100+", NA), exclude = NULL))
})

test_that("date_to_age_group_year gives correct answers with leap years", {
    expect_identical(date_to_age_group_year(date = c("2001-02-27",
                                                     "2001-02-28",
                                                     "2004-02-27",
                                                     "2004-02-28",
                                                     "2004-02-29"),
                                            dob = "2000-02-29"),
                     factor(c(0, 1, 3, 4, 4), levels = c(0:99, "100+")))
})


## date_to_age_group_multi --------------------------------------------------

test_that("date_to_age_group_multi gives correct answers with valid inputs", {
    expect_identical(date_to_age_group_multi(date = c("2000-01-01",
                                                      "2010-01-01",
                                                      "2004-12-31"),
                                             dob = "2000-01-01",
                                             width = 5),
                     factor(c("0-4", "10-14", "0-4"),
                            levels = c(paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"),
                                       "100+")))
    expect_identical(date_to_age_group_multi(date = c("2000-01-01",
                                                      "2010-01-01",
                                                      "2004-12-31"),
                                             dob = "2000-01-01",
                                             age_max = 10),
                     factor(c("0-4", "10+", "0-4"),
                            levels = c("0-4", "5-9", "10+")))
    expect_identical(date_to_age_group_multi(date = c("2000-01-01",
                                                      "2010-01-01",
                                                      "2004-12-31"),
                                             dob = "2000-01-01",
                                             width = 10,
                                             age_max = 10),
                     factor(c("0-9", "10+", "0-9"), levels = c("0-9", "10+")))
    expect_identical(date_to_age_group_multi(date = c("2000-01-01",
                                                      "2010-01-01",
                                                      "2004-12-31"),
                                             dob = "2000-01-01",
                                             width = 5,
                                             age_max = Inf),
                     factor(c("0-4", "10+", "0-4"),
                            levels = c("0-4", "5-9", "10+")))
    expect_identical(date_to_age_group_multi(date = c("2000-01-01",
                                                      "2010-01-01",
                                                      "2004-12-31"),
                                             dob = "2000-01-01",
                                             width = 5,
                                             age_max = Inf,
                                             open_right = FALSE),
                     factor(c("0-4", "10-14", "0-4"),
                            levels = c("0-4", "5-9", "10-14")))
})


## date_to_age_group_lifetab --------------------------------------------------

test_that("date_to_age_group_lifetab gives correct answers with valid inputs", {
    expect_identical(date_to_age_group_lifetab(date = c("2000-01-01",
                                                        "2010-01-01",
                                                        "2004-12-31"),
                                               dob = "2000-01-01"),
                     factor(c("0", "10-14", "1-4"),
                            levels = c("0", "1-4", paste(seq(5, 95, 5), seq(9, 99, 5), sep = "-"),
                                       "100+")))
    expect_identical(date_to_age_group_lifetab(date = c("2000-01-01",
                                                        "2010-01-01",
                                                        "2004-12-31"),
                                               dob = "2000-01-01",
                                               age_max = 10),
                     factor(c("0", "10+", "1-4"),
                            levels = c("0", "1-4", "5-9", "10+")))
})

## date_to_age_group_fert --------------------------------------------------

test_that("date_to_age_group_fert gives correct answers with valid inputs", {
    expect_identical(date_to_age_group_fert(date = c("2015-01-01",
                                                     "2025-01-01",
                                                     "2029-12-31"),
                                            dob = "2000-01-01"),
                     factor(c("15-19", "25-29", "25-29"),
                            levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")))
    expect_identical(date_to_age_group_fert(date = c("2015-01-01",
                                                     "2025-01-01",
                                                     "2029-12-31"),
                                            dob = "2000-01-01",
                                            age_min = 10,
                                            age_max = 55),
                     factor(c("15-19", "25-29", "25-29"),
                            levels = c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")))
    expect_identical(date_to_age_group_fert(date = c("2015-01-01",
                                                     "2025-01-01",
                                                     "2029-12-31"),
                                            dob = "2000-01-01",
                                            age_min = 15,
                                            age_max = 50,
                                            width = 1),
                     factor(c("15", "25", "29"),
                            levels = 15:49))
    expect_identical(date_to_age_group_fert(date = c("2015-01-01",
                                                     "2025-01-01",
                                                     "2049-12-31"),
                                            dob = "2000-01-01",
                                            age_min = 20,
                                            age_max = 40,
                                            recode_up = TRUE,
                                            recode_down = TRUE),
                     factor(c("20-24", "25-29", "35-39"),
                            levels = c("20-24", "25-29", "30-34", "35-39")))
})

test_that("date_to_age_group_fert throws correct errors with invalid inputs", {
    expect_error(date_to_age_group_fert(date = c("2015-01-01",
                                                 "2025-01-01",
                                                 "2029-12-31"),
                                        dob = "2000-01-01",
                                        width = 3),
                 "difference between 'age_max' \\[50\\] and 'age_min' \\[15\\] not divisible by 'width' \\[3\\]")
    expect_error(date_to_age_group_fert(date = c("2015-01-01",
                                                 "2025-01-01",
                                                 "2029-12-31"),
                                        dob = "2000-01-01",
                                        age_min = 20),
                 paste("'date' of \"2015-01-01\" and 'dob' of \"2000-01-01\" imply age of 15,",
                       "but 'age_min' is 20 and 'recode_up' is FALSE"))
    expect_error(date_to_age_group_fert(date = c("2045-01-01",
                                                 "2025-01-01",
                                                 "2029-12-31"),
                                        dob = "2000-01-01",
                                        age_max = 45),
                 paste("'date' of \"2045-01-01\" and 'dob' of \"2000-01-01\" imply age of 45,",
                       "but 'age_max' is 45 and 'recode_down' is FALSE"))
})


## date_to_age_group_custom ---------------------------------------------------

test_that("date_to_age_group_custom gives correct answers with valid inputs", {
    expect_identical(date_to_age_group_custom(date = c("2003-01-01",
                                                       "2025-01-01",
                                                       "2039-12-31"),
                                              dob = "2000-01-01",
                                              breaks = c(0, 10, 30)),
                     factor(c("0-9", "10-29", "30+"),
                            levels = c("0-9", "10-29", "30+")))
    expect_identical(date_to_age_group_custom(date = c("2000-06-01",
                                                       "2015-01-01",
                                                       "2016-12-31"),
                                              dob = "2000-01-01",
                                              breaks = c(0, 1, 30),
                                              open_right = FALSE),
                     factor(c("0", "1-29", "1-29"),
                            levels = c("0", "1-29")))
    expect_identical(date_to_age_group_custom(date = c("2005-06-01",
                                                       "2015-01-01",
                                                       "2016-12-31"),
                                              dob = "2000-01-01",
                                              breaks = c(5, 10, 30),
                                              open_right = FALSE),
                     factor(c("5-9", "10-29", "10-29"),
                            levels = c("5-9", "10-29")))
})

test_that("date_to_age_group_fert throws correct errors with invalid inputs", {
    expect_error(date_to_age_group_custom(date = c("2001-06-01",
                                                       "2015-01-01",
                                                       "2016-12-31"),
                                              dob = "2000-01-01",
                                          breaks = c(5, 10, 30)),
                 "'date' of \"2001-06-01\" and 'dob' of \"2000-01-01\" imply age of 1, but minimum value for 'breaks' is 5")
    expect_error(date_to_age_group_custom(date = c("2001-06-01",
                                                       "2015-01-01",
                                                       "2026-12-31"),
                                              dob = "2000-01-01",
                                          breaks = c(0, 10, 20),
                                          open_right = FALSE),
                 "'date' of \"2026-12-31\" and 'dob' of \"2000-01-01\" imply age of 26, but 'open_right' is FALSE and maximum value for 'breaks' is 20")
})




