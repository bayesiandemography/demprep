
context("make_labels_age")

## make_labels_age --------------------------------------------------

test_that("make_labels_age gives correct answers with valid input", {
    expect_identical(make_labels_age(breaks = 0:5,
                                     open_last = FALSE),
                     c("0", "1", "2", "3", "4"))
    expect_identical(make_labels_age(breaks = 0:5,
                                     open_last = TRUE),
                     c("0", "1", "2", "3", "4", "5+"))
    expect_identical(make_labels_age(breaks = c(0L, 1L, 5L),
                                     open_last = TRUE),
                     c("0", "1-4", "5+"))
    expect_identical(make_labels_age(breaks = integer(),
                                     open_last = FALSE),
                     character())
    expect_identical(make_labels_age(breaks = c(0L, 1L, 5L),
                                     open_last = TRUE,
                                     include_na = TRUE),
                     c("0", "1-4", "5+", NA))
    expect_identical(make_labels_age(breaks = integer(),
                                     open_last = FALSE,
                                     include_na = TRUE),
                     as.character(NA))
})

test_that("make_labels_age throws correct error with invalid input", {
    expect_error(make_labels_age(breaks = integer(),
                                 open_last = TRUE),
                 "'breaks' has length 0 but 'open_last' is TRUE")
})


## make_labels_age_quarter --------------------------------------------------

test_that("make_labels_age_quarter gives correct answers with valid input", {
    expect_identical(make_labels_age_quarter(),
                     c(paste0(0:399, "q"), "400q+"))
    expect_identical(make_labels_age_quarter(break_max = 5,
                                             open_last = FALSE,
                                             include_na = TRUE),
                     c("0q", "1q", "2q", "3q", "4q", NA))
})


## make_labels_age_month --------------------------------------------------

test_that("make_labels_age_month gives correct answers with valid input", {
    expect_identical(make_labels_age_month(),
                     c(paste0(0:1199, "m"), "1200m+"))
    expect_identical(make_labels_age_month(break_max = 5,
                                           open_last = FALSE,
                                           include_na = TRUE),
                     c("0m", "1m", "2m", "3m", "4m", NA))
})
