
context("helper-make_labels")

## make_labels_age_group_month_quarter ----------------------------------------

test_that("'make_labels_age_group_month_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_last = FALSE,
                                                         unit = "month",
                                                         include_na = FALSE),
                     c("0m", "1m", "2m", "3m", "4m"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_last = FALSE,
                                                         unit = "quarter",
                                                         include_na = FALSE),
                     c("0q", "1q", "2q", "3q", "4q"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_last = TRUE,
                                                         unit = "month",
                                                         include_na = FALSE),
                     c("0m", "1m", "2m", "3m", "4m", "5m+"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_last = TRUE,
                                                         unit = "quarter",
                                                         include_na = TRUE),
                     c("0q", "1q", "2q", "3q", "4q", "5q+", NA))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 0,
                                                         open_last = TRUE,
                                                         unit = "month",
                                                         include_na = FALSE),
                     "0m+")
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 0,
                                                         open_last = TRUE,
                                                         unit = "quarter",
                                                         include_na = TRUE),
                     c("0q+", NA))
})

test_that("'make_labels_age_group_month_quarter' gives correct error with invalid inputs", {
    expect_error(make_labels_age_group_month_quarter(break_min = 0,
                                                     break_max = 0,
                                                     open_last = FALSE,
                                                     unit = "month",
                                                     include_na = FALSE),
                 "'break_max' \\[0\\] is non-positive")
    expect_error(make_labels_age_group_month_quarter(break_min = 0,
                                                     break_max = 5,
                                                     open_last = FALSE,
                                                     unit = "wrong",
                                                     include_na = FALSE),
                 "can't handle unit 'wrong'")
})


## make_labels_cohort_month_quarter ----------------------------------------

test_that("'make_labels_cohort_month_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_cohort_month_quarter(break_min = "2019-01-01",
                                                      break_max = "2019-04-01",
                                                      open_first = FALSE,
                                                      unit = "month",
                                                      include_na = FALSE),
                     c("2019 Jan", "2019 Feb", "2019 Mar"))
    expect_identical(make_labels_cohort_month_quarter(break_min = "2019-01-01",
                                                      break_max = "2020-01-01",
                                                      open_first = FALSE,
                                                      unit = "quarter",
                                                      include_na = FALSE),
                     c("2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4"))
    expect_identical(make_labels_cohort_month_quarter(break_min = "2019-07-01",
                                                      break_max = "2020-07-01",
                                                      open_first = TRUE,
                                                      unit = "quarter",
                                                      include_na = TRUE),
                     c("<2019 Q3", "2019 Q3", "2019 Q4", "2020 Q1", "2020 Q2", NA))
    expect_identical(make_labels_cohort_month_quarter(break_min = "2019-07-01",
                                                      break_max = "2019-07-01",
                                                      open_first = TRUE,
                                                      unit = "quarter",
                                                      include_na = FALSE),
                     "<2019 Q3")
})

test_that("'make_labels_cohort_month_quarter' gives correct error with invalid inputs", {
    expect_error(make_labels_cohort_month_quarter(break_min = "2001-03-01",
                                                  break_max = "2001-04-01",
                                                  open_first = FALSE,
                                                  unit = "wrong",
                                                  include_na = FALSE),
                 "value for 'unit' \\[\"wrong\"\\] is not a permitted time unit")
    expect_error(make_labels_cohort_month_quarter(break_min = "2019-07-01",
                                                      break_max = "2019-07-01",
                                                      open_first = FALSE,
                                                      unit = "quarter",
                                                      include_na = FALSE),
                 "'break_max' \\[2019-07-01\\] is less than or equal to 'break_min' \\[2019-07-01\\]")
})


## make_labels_period_month_quarter ----------------------------------------

test_that("'make_labels_period_month_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_period_month_quarter(break_min = "2019-01-01",
                                                      break_max = "2019-04-01",
                                                      unit = "month",
                                                      include_na = FALSE),
                     c("2019 Jan", "2019 Feb", "2019 Mar"))
    expect_identical(make_labels_period_month_quarter(break_min = "2019-01-01",
                                                      break_max = "2020-01-01",
                                                      unit = "quarter",
                                                      include_na = FALSE),
                     c("2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4"))
})

test_that("'make_labels_period_month_quarter' gives correct error with invalid inputs", {
    expect_error(make_labels_period_month_quarter(break_min = "2001-03-01",
                                                  break_max = "2001-04-01",
                                                  unit = "wrong",
                                                  include_na = FALSE),
                 "value for 'unit' \\[\"wrong\"\\] is not a permitted time unit")
    expect_error(make_labels_period_month_quarter(break_min = "2019-07-01",
                                                  break_max = "2019-07-01",
                                                  unit = "quarter",
                                                  include_na = FALSE),
                 "'break_max' \\[2019-07-01\\] is less than or equal to 'break_min' \\[2019-07-01\\]")
})

