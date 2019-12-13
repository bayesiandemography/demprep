
context("make_labels_period")

## make_labels_period ----------------------------------------------------

test_that("make_labels_period gives correct answers with valid input", {
    expect_identical(make_labels_period(breaks = as.Date(c("2000-01-01",
                                                           "2001-01-01",
                                                           "2002-01-01"))),
                     c("2000", "2001"))
    expect_identical(make_labels_period(breaks = as.Date(c("2000-01-01",
                                                           "2001-01-01",
                                                           "2002-01-01")),
                                        label_year_start = FALSE),
                     c("2000", "2001"))
    expect_identical(make_labels_period(breaks = as.Date(c("2000-02-01",
                                                           "2001-02-01",
                                                           "2002-02-01"))),
                     c("2000", "2001"))
    expect_identical(make_labels_period(breaks = as.Date(c("2000-02-01",
                                                           "2001-02-01",
                                                           "2002-02-01")),
                                        label_year_start = FALSE),
                     c("2001", "2002"))
    expect_identical(make_labels_period(breaks = as.Date(c("2001-07-01",
                                                           "2006-07-01",
                                                           "2011-07-01"))),
                     c("2001-2006", "2006-2011"))
    expect_identical(make_labels_period(breaks = as.Date(c("2001-07-01",
                                                           "2006-07-01",
                                                           "2011-07-01")),
                                        label_year_start = FALSE),
                     c("2001-2006", "2006-2011"))
    expect_identical(make_labels_period(breaks = as.Date(character())),
                     character())
})


## make_labels_period_quarter --------------------------------------------------

test_that("make_labels_period_quarter gives correct answers with valid input", {
    expect_identical(make_labels_period_quarter(break_min = "2000-01-01",
                                                break_max = "2000-07-01",
                                                include_na = TRUE),
                     c("2000 Q1", "2000 Q2", NA))
})


## make_labels_period_month ---------------------------------------------------

test_that("make_labels_period_month gives correct answers with valid input", {
    expect_identical(make_labels_period_month(break_min = "2000-01-01",
                                              break_max = "2000-05-01",
                                              include_na = TRUE),
                     c("2000 Jan", "2000 Feb", "2000 Mar", "2000 Apr", NA))
})
