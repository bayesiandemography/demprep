
context("impute")

## impute_dob ------------------------------------------------------

test_that("impute_dob gives correct answers with valid inputs", {
    impute_dob <- demprep:::impute_dob
    n_sample <- 1000
    set.seed(0)
    ## year and month
    ans_obtained <- impute_dob(date = rep("2001-04-01", n_sample),
                               age_years = rep(1, n_sample),
                               age_months = rep(1, n_sample))
    expect_false(anyNA(ans_obtained))
    expect_true(identical(min(ans_obtained), as.Date("2000-02-02")))
    expect_true(as.Date("2000-02-29") %in% ans_obtained)
    expect_true(identical(max(ans_obtained), as.Date("2000-03-01")))
    ## year only - year > 0, leap year
    ans_obtained <- impute_dob(date = rep("2000-02-29", n_sample),
                               age_years = rep(1, n_sample))
    expect_false(anyNA(ans_obtained))
    expect_true(identical(min(ans_obtained), as.Date("1998-03-01")))
    expect_true(identical(max(ans_obtained), as.Date("1999-02-28")))
    ## year only - year == 0
    ans_obtained <- impute_dob(date = rep("2000-05-20", n_sample),
                               age_years = rep(0, n_sample))
    expect_false(anyNA(ans_obtained))
    expect_true(identical(min(ans_obtained), as.Date("1999-05-21")))
    expect_true(identical(max(ans_obtained), as.Date("2000-05-20")))
    ## month only - month > 0
    ans_obtained <- impute_dob(date = rep("2000-05-20", n_sample),
                               age_months = rep(12, n_sample))
    expect_false(anyNA(ans_obtained))
    expect_true(identical(min(ans_obtained), as.Date("1999-04-21")))
    expect_true(identical(max(ans_obtained), as.Date("1999-05-20")))
    ## month only - month == 0
    ans_obtained <- impute_dob(date = rep("2000-05-20", n_sample),
                               age_months = rep(0, n_sample))
    expect_false(anyNA(ans_obtained))
    expect_true(identical(min(ans_obtained), as.Date("2000-04-21")))
    expect_true(identical(max(ans_obtained), as.Date("2000-05-20")))
    ## month only - month == 0, leap year
    ans_obtained <- impute_dob(date = rep("2000-03-28", n_sample),
                               age_months = rep(0, n_sample))
    expect_false(anyNA(ans_obtained))
    expect_true(identical(min(ans_obtained), as.Date("2000-02-29")))
    expect_true(identical(max(ans_obtained), as.Date("2000-03-28")))
    ## length 0
    ans_obtained <- impute_dob(date = character(),
                               age_years = integer())
    ans_expected <- as.Date(character())
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- impute_dob(date = character(),
                               age_months = integer())
    ans_expected <- as.Date(character())
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- impute_dob(date = character(),
                               age_years = integer(),
                               age_months = integer())
    ans_expected <- as.Date(character())
    expect_identical(ans_obtained, ans_expected)
    ## contains NAs
    ans_obtained <- impute_dob(date = c("2000-05-20", NA),
                               age_years = c(0, 0))
    expect_identical(is.na(ans_obtained), c(FALSE, TRUE))
    ans_obtained <- impute_dob(date = c("2000-05-20", "2000-01-01"),
                               age_months = c(0, NA))
    expect_identical(is.na(ans_obtained), c(FALSE, TRUE))
    ans_obtained <- impute_dob(date = NA,
                               age_years = NA)
    expect_identical(is.na(ans_obtained), TRUE)
})

test_that("impute_dob throws correct errors with invalid inputs", {
    expect_error(impute_dob(date = "2015-01-01"),
                 "one or both of 'age_years' and 'age_months' must be supplied")
})

test_that("can impute dates of birth, then convert back to ages and get original values", {
    set.seed(0)
    n_sample <- 1000
    s <- seq.Date(from = as.Date("1980-01-01"),
                  to = as.Date("2020-01-01"),
                  by = "day")
    date <- sample(s,
                   size = n_sample,
                   replace = TRUE)
    ## year only
    age_years <- sample(0:100,
                        size = n_sample,
                        replace = TRUE)
    dob <- impute_dob(date = date,
                      age_years = age_years)
    age_years_inferred <- date_to_age_year(date = date,
                                           dob = dob)
    expect_identical(age_years, age_years_inferred)
    ## month only
    age_months <- sample(0:1200,
                         size = n_sample,
                         replace = TRUE)
    dob <- impute_dob(date = date,
                      age_months = age_months)
    age_months_inferred <- date_to_age_month(date = date,
                                             dob = dob)
    expect_identical(age_months, age_months_inferred)
    ## year and month
    age_years <- sample(0:90,
                        size = n_sample,
                        replace = TRUE)
    age_months <- sample(0:11,
                         size = n_sample,
                         replace = TRUE)
    dob <- impute_dob(date = date,
                      age_years = age_years,
                      age_months = age_months)
    age_months_inferred <- date_to_age_month(date = date,
                                             dob = dob)
    expect_identical(12L * age_years + age_months,
                     age_months_inferred)
})



