
context("helper-impute")

## impute_date_day ------------------------------------------------------

test_that("impute_date_day gives correct answers with valid inputs", {
    impute_date_day <- demprep:::impute_date_day
    set.seed(0)
    ans_obtained <- impute_date_day(year = rep(2000:2001, each = 1000),
                                    month = sample(1:12, size = 2000, replace = TRUE))
    expect_false(anyNA(ans_obtained))
    expect_true(all(ans_obtained >= as.Date("2000-01-01")))
    expect_true(all(ans_obtained <= as.Date("2001-12-31")))
})


## impute_date_month ------------------------------------------------------

test_that("impute_date_month gives correct answers with valid inputs", {
    impute_date_month <- demprep:::impute_date_month
    set.seed(0)
    ans_obtained <- impute_date_month(year = rep(2000:2001, each = 1000),
                                      day = sample(1:31, size = 2000, replace = TRUE))
    expect_false(anyNA(ans_obtained))
    expect_true(all(ans_obtained >= as.Date("2000-01-01")))
    expect_true(all(ans_obtained <= as.Date("2001-12-31")))
})


## impute_date_month_day ------------------------------------------------------

test_that("impute_date_month_day gives correct answers with valid inputs", {
    impute_date_month_day <- demprep:::impute_date_month_day
    set.seed(0)
    ans_obtained <- impute_date_month_day(year = rep(2000:2001, each = 1000))
    expect_false(anyNA(ans_obtained))
    expect_true(all(ans_obtained >= as.Date("2000-01-01")))
    expect_true(all(ans_obtained <= as.Date("2001-12-31")))
})


    
    
    
