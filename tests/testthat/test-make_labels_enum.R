
context("make_labels_enum")

## make_labels_enum --------------------------------------------------

test_that("make_labels_enum gives correct answers with valid input", {
    expect_identical(make_labels_enum(breaks = 0:5,
                                      open_last = FALSE),
                     c("0", "1", "2", "3", "4"))
    expect_identical(make_labels_enum(breaks = 0:5,
                                      open_first = TRUE,
                                      open_last = TRUE),
                     c("<0", "0", "1", "2", "3", "4", "5+"))
    expect_identical(make_labels_enum(breaks = c(-5, 0, 1, 5),
                                      open_last = TRUE),
                     c("-5--1", "0", "1-4", "5+"))
    expect_identical(make_labels_enum(breaks = integer(),
                                      open_last = FALSE),
                     character())
    expect_identical(make_labels_enum(breaks = c(0L, 1L, 5L),
                                      open_last = TRUE,
                                      include_na = TRUE),
                     c("0", "1-4", "5+", NA))
    expect_identical(make_labels_enum(breaks = integer(),
                                      open_last = FALSE,
                                      include_na = TRUE),
                     as.character(NA))
})

test_that("make_labels_enum throws correct error with invalid input", {
    expect_error(make_labels_enum(breaks = integer(),
                                  open_last = TRUE),
                 "'breaks' has length 0 but 'open_last' is TRUE")
    expect_error(make_labels_enum(breaks = 100),
                 "'breaks' has length 1 but 'open_first' and 'open_last' are both FALSE")
})
