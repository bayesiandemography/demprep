
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
                     factor(c(0, 10, 4), levels = 0:100))
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


