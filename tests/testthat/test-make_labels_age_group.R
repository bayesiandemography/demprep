
context("make_labels_age_group")

## make_labels_age_group_year --------------------------------------------------

test_that("make_labels_age_group_year gives correct answers with valid input", {
    expect_identical(make_labels_age_group_year(breaks = 0:5,
                                                open_left = FALSE,
                                                open_right = FALSE),
                     c("0", "1", "2", "3", "4"))
    expect_identical(make_labels_age_group_year(breaks = 0:5,
                                                open_left = TRUE,
                                                open_right = FALSE),
                     c("<0", "0", "1", "2", "3", "4"))
    expect_identical(make_labels_age_group_year(breaks = 0:5,
                                                open_left = TRUE,
                                                open_right = TRUE),
                     c("<0", "0", "1", "2", "3", "4", "5+"))
    expect_identical(make_labels_age_group_year(breaks = c(0L, 1L, 5L),
                                                open_left = FALSE,
                                                open_right = TRUE),
                     c("0", "1-4", "5+"))
    expect_identical(make_labels_age_group_year(breaks = integer(),
                                                open_left = FALSE,
                                                open_right = FALSE),
                     character())
    expect_identical(make_labels_age_group_year(breaks = c(0L, 1L, 5L),
                                                open_left = FALSE,
                                                open_right = TRUE,
                                                include_na = TRUE),
                     c("0", "1-4", "5+", NA))
    expect_identical(make_labels_age_group_year(breaks = integer(),
                                                open_left = FALSE,
                                                open_right = FALSE,
                                                include_na = TRUE),
                     as.character(NA))
})

test_that("make_labels_age_group_year throws correct error with invalid input", {
    expect_error(make_labels_age_group_year(breaks = integer(),
                                            open_left = TRUE,
                                            open_right = FALSE),
                 "'breaks' has length 0 but 'open_left' is TRUE")
    expect_error(make_labels_age_group_year(breaks = integer(),
                                            open_left = FALSE,
                                            open_right = TRUE),
                 "'breaks' has length 0 but 'open_right' is TRUE")
})

