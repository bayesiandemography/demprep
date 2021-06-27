
## impute_date_day ------------------------------------------------------

test_that("impute_date_day gives correct answers with valid inputs", {
    impute_date_day <- demprep:::impute_date_day
    n_sample <- 1000
    set.seed(0)
    ans_obtained <- impute_date_day(year = rep(2000:2001, each = n_sample),
                                    month = sample(1:12, size = 2 * n_sample, replace = TRUE))
    expect_false(anyNA(ans_obtained))
    expect_identical(min(ans_obtained), as.Date("2000-01-01"))
    expect_identical(max(ans_obtained), as.Date("2001-12-31"))
    expect_identical(impute_date_day(year = 2000, month = NA_integer_),
                     as.Date(NA_character_))
    expect_identical(impute_date_day(year = NA_real_, month = 1),
                     as.Date(NA_character_))
    expect_identical(impute_date_day(year = NA_real_, month = NA_integer_),
                     as.Date(NA_character_))
})


## impute_date_month ------------------------------------------------------

test_that("impute_date_month gives correct answers with valid inputs", {
    impute_date_month <- demprep:::impute_date_month
    n_sample <- 1000
    set.seed(0)
    ans_obtained <- impute_date_month(year = rep(2000:2001, each = n_sample),
                                      day = sample(1:31, size = 2 * n_sample, replace = TRUE))
    expect_false(anyNA(ans_obtained))
    expect_identical(min(ans_obtained), as.Date("2000-01-01"))
    expect_identical(max(ans_obtained), as.Date("2001-12-31"))
    expect_identical(impute_date_month(year = 2000, day = NA_integer_),
                     as.Date(NA_character_))
    expect_identical(impute_date_month(year = NA_real_, day = 30),
                     as.Date(NA_character_))
    expect_identical(impute_date_month(year = NA_real_, day = NA_integer_),
                     as.Date(NA_character_))
})


## impute_date_month_day ------------------------------------------------------

test_that("impute_date_month_day gives correct answers with valid inputs", {
    impute_date_month_day <- demprep:::impute_date_month_day
    n_sample <- 1000
    set.seed(0)
    ans_obtained <- impute_date_month_day(year = rep(2000:2001, each = n_sample))
    expect_false(anyNA(ans_obtained))
    expect_identical(min(ans_obtained), as.Date("2000-01-01"))
    expect_identical(max(ans_obtained), as.Date("2001-12-31"))
    expect_identical(is.na(impute_date_month_day(year = c(2000, NA))),
                     c(FALSE, TRUE))
})


    
    
    
