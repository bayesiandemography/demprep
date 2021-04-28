
context("helper-labels")

## make_labels_complete_calendar_month ----------------------------------------

test_that("'make_labels_complete_calendar_month' gives correct answer with valid inputs", {
    expect_identical(make_labels_complete_calendar_month(break_min = as.Date("2019-01-01"),
                                                         break_max = as.Date("2019-04-01"),
                                                         open_first = FALSE,
                                                         include_na = TRUE),
                     c("2019 Jan", "2019 Feb", "2019 Mar", NA))
    expect_identical(make_labels_complete_calendar_month(break_min = as.Date("2019-10-01"),
                                                         break_max = as.Date("2020-01-01"),
                                                         open_first = FALSE,
                                                         include_na = FALSE),
                     c("2019 Oct", "2019 Nov", "2019 Dec"))
    expect_identical(make_labels_complete_calendar_month(break_min = as.Date("2019-10-01"),
                                                         break_max = as.Date("2020-01-01"),
                                                         open_first = TRUE,
                                                         include_na = FALSE),
                     c("<2019 Oct", "2019 Oct", "2019 Nov", "2019 Dec"))
    expect_identical(make_labels_complete_calendar_month(break_min = as.Date("2019-07-01"),
                                                         break_max = as.Date("2019-07-01"),
                                                         open_first = TRUE,
                                                         include_na = FALSE),
                     "<2019 Jul")
})

## make_labels_complete_calendar_quarter --------------------------------------

test_that("'make_labels_complete_calendar_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_complete_calendar_quarter(break_min = as.Date("2019-01-01"),
                                                           break_max = as.Date("2019-04-01"),
                                                           open_first = FALSE,
                                                           include_na = FALSE),
                     "2019 Q1")
    expect_identical(make_labels_complete_calendar_quarter(break_min = as.Date("2019-01-01"),
                                                           break_max = as.Date("2020-01-01"),
                                                           open_first = FALSE,
                                                           include_na = FALSE),
                     c("2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4"))
    expect_identical(make_labels_complete_calendar_quarter(break_min = as.Date("2019-07-01"),
                                                           break_max = as.Date("2020-07-01"),
                                                           open_first = TRUE,
                                                           include_na = TRUE),
                     c("<2019 Q3", "2019 Q3", "2019 Q4", "2020 Q1", "2020 Q2", NA))
    expect_identical(make_labels_complete_calendar_quarter(break_min = as.Date("2019-07-01"),
                                                           break_max = as.Date("2019-07-01"),
                                                           open_first = TRUE,
                                                           include_na = TRUE),
                     c("<2019 Q3", NA))
})
