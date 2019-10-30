
context("make_labels_age_group")

## make_labels_age_group --------------------------------------------------

test_that("make_labels_age_group gives correct answers with valid input", {
    expect_identical(make_labels_age_group(breaks = 0:5,
                                           open_first = FALSE,
                                           open_last = FALSE),
                     c("0", "1", "2", "3", "4"))
    expect_identical(make_labels_age_group(breaks = 0:5,
                                           open_first = TRUE,
                                           open_last = FALSE),
                     c("<0", "0", "1", "2", "3", "4"))
    expect_identical(make_labels_age_group(breaks = 0:5,
                                           open_first = TRUE,
                                           open_last = TRUE),
                     c("<0", "0", "1", "2", "3", "4", "5+"))
    expect_identical(make_labels_age_group(breaks = c(0L, 1L, 5L),
                                           open_first = FALSE,
                                           open_last = TRUE),
                     c("0", "1-4", "5+"))
    expect_identical(make_labels_age_group(breaks = integer(),
                                           open_first = FALSE,
                                           open_last = FALSE),
                     character())
    expect_identical(make_labels_age_group(breaks = c(0L, 1L, 5L),
                                           open_first = FALSE,
                                           open_last = TRUE,
                                           include_na = TRUE),
                     c("0", "1-4", "5+", NA))
    expect_identical(make_labels_age_group(breaks = integer(),
                                           open_first = FALSE,
                                           open_last = FALSE,
                                           include_na = TRUE),
                     as.character(NA))
})

test_that("make_labels_age_group throws correct error with invalid input", {
    expect_error(make_labels_age_group(breaks = integer(),
                                       open_first = TRUE,
                                       open_last = FALSE),
                 "'breaks' has length 0 but 'open_first' is TRUE")
    expect_error(make_labels_age_group(breaks = integer(),
                                       open_first = FALSE,
                                       open_last = TRUE),
                 "'breaks' has length 0 but 'open_last' is TRUE")
})


## make_labels_age_group_quarter --------------------------------------------------

test_that("make_labels_age_group_quarter gives correct answers with valid input", {
    expect_identical(make_labels_age_group_quarter(),
                     c(paste0(0:399, "q"), "400q+"))
    expect_identical(make_labels_age_group_quarter(break_max = 5,
                                                   open_first = TRUE,
                                                   open_last = FALSE,
                                                   include_na = TRUE),
                     c("<0q", "0q", "1q", "2q", "3q", "4q", NA))
})


## make_labels_age_group_month --------------------------------------------------

test_that("make_labels_age_group_month gives correct answers with valid input", {
    expect_identical(make_labels_age_group_month(),
                     c(paste0(0:1199, "m"), "1200m+"))
    expect_identical(make_labels_age_group_month(break_max = 5,
                                                 open_first = TRUE,
                                                 open_last = FALSE,
                                                 include_na = TRUE),
                     c("<0m", "0m", "1m", "2m", "3m", "4m", NA))
})
