
context("make_labels_cohort")

## make_labels_cohort ----------------------------------------------------

test_that("make_labels_cohort gives correct answers with valid input", {
    expect_identical(make_labels_cohort(breaks = as.Date(c("2000-01-01",
                                                           "2001-01-01",
                                                           "2002-01-01"))),
                     c("2000", "2001"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2000-01-01",
                                                           "2001-01-01",
                                                           "2002-01-01")),
                                        label_year_start = FALSE),
                     c("2000", "2001"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2000-02-01",
                                                           "2001-02-01",
                                                           "2002-02-01"))),
                     c("2000", "2001"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2000-02-01",
                                                           "2001-02-01",
                                                           "2002-02-01")),
                                        label_year_start = FALSE),
                     c("2001", "2002"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2001-07-01",
                                                           "2006-07-01",
                                                           "2011-07-01"))),
                     c("2001-2006", "2006-2011"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2001-07-01",
                                                           "2006-07-01",
                                                           "2011-07-01")),
                                        label_year_start = FALSE),
                     c("2001-2006", "2006-2011"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2001-07-01",
                                                           "2006-07-01",
                                                           "2011-07-01")),
                                        open_first = TRUE,
                                        include_na = TRUE),
                     c("<2001", "2001-2006", "2006-2011", NA))
    expect_identical(make_labels_cohort(breaks = as.Date(character())),
                     character())
    expect_identical(make_labels_cohort(breaks = as.Date(c("2001-01-01",
                                                           "2002-01-01")),
                                        open_first = TRUE),
                     c("<2001", "2001"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2001-07-01",
                                                           "2002-07-01")),
                                        open_first = TRUE),
                     c("<2001", "2001"))
    expect_identical(make_labels_cohort(breaks = as.Date("2001-07-01"),
                                        open_first = TRUE),
                     "<2001")
    expect_identical(make_labels_cohort(breaks = as.Date(c("2001-07-01",
                                                           "2006-07-01")),
                                        open_first = TRUE),
                     c("<2001", "2001-2006"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2001-07-01",
                                                           "2002-07-01",
                                                           "2006-07-01")),
                                        open_first = TRUE),
                     c("<2001", "2001-2002", "2002-2006"))
    expect_identical(make_labels_cohort(breaks = as.Date(c("2001-07-01",
                                                           "2002-07-01",
                                                           "2006-07-01",
                                                           "2007-07-01")),
                                        open_first = TRUE),
                     c("<2001", "2001-2002", "2002-2006", "2006-2007"))
})

test_that("make_labels_cohort throws correct error with invalid input", {
    expect_error(make_labels_cohort(breaks = character(),
                                    open_first = TRUE),
                 "'breaks' has length 0 but 'open_first' is TRUE")
    expect_error(make_labels_cohort(breaks = c("2000-01-01",
                                               "2001-01-01",
                                               "2002-01-01"),
                                    label_year_start = FALSE,
                                    open_first = TRUE),
                 "'open_first' is TRUE but 'label_year_start' is FALSE")
})


## make_labels_cohort_quarter --------------------------------------------------

test_that("make_labels_cohort_quarter gives correct answers with valid input", {
    expect_identical(make_labels_cohort_quarter(break_min = "2000-01-01",
                                                break_max = "2000-07-01",
                                                open_first = TRUE,
                                                include_na = TRUE),
                     c("<2000 Q1", "2000 Q1", "2000 Q2", NA))
})


## make_labels_cohort_month ---------------------------------------------------

test_that("make_labels_cohort_month gives correct answers with valid input", {
    expect_identical(make_labels_cohort_month(break_min = "2000-01-01",
                                              break_max = "2000-05-01",
                                              open_first = TRUE,
                                              include_na = TRUE),
                     c("<2000 Jan", "2000 Jan", "2000 Feb", "2000 Mar", "2000 Apr", NA))
})
