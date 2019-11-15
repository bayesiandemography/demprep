
context("infer_dimscale")

## infer_dimscale_period_month ------------------------------------------------

test_that("infer_dimscale_period_month gives correct answers with valid input", {
    break_min <- as.Date("2000-01-01")
    break_max <- as.Date("2001-04-01")
    open_first <- TRUE
    open_last <- TRUE
    include_na <- TRUE
    labels <- make_labels_period_month(break_min = break_min,
                                       break_max = break_max,
                                       open_first = open_first,
                                       open_last = open_last,
                                       include_na = include_na)
    ans_obtained <- infer_dimscale_period_month(labels)
    ans_expected <- list(break_min = break_min,
                         break_max = break_max,
                         open_first = open_first,
                         open_last = open_last,
                         include_na = include_na)
    expect_identical(ans_obtained, ans_expected)
    labels <- rev(rep(labels, each = 2))
    ans_obtained <- infer_dimscale_period_month(labels)
    ans_expected <- list(break_min = break_min,
                         break_max = break_max,
                         open_first = open_first,
                         open_last = open_last,
                         include_na = include_na)
    expect_identical(ans_obtained, ans_expected)
    break_min <- as.Date("2000-01-01")
    break_max <- as.Date("2000-02-01")
    open_first <- FALSE
    open_last <- FALSE
    include_na <- FALSE
    labels <- make_labels_period_month(break_min = break_min,
                                       break_max = break_max,
                                       open_first = open_first,
                                       open_last = open_last,
                                       include_na = include_na)
    ans_obtained <- infer_dimscale_period_month(labels)
    ans_expected <- list(break_min = break_min,
                         break_max = break_max,
                         open_first = open_first,
                         open_last = open_last,
                         include_na = include_na)
    expect_identical(ans_obtained, ans_expected)
})

## test_that("infer_dimscale_period_month throws correct error with invalid input", {
##     expect_error(make_labels_period(breaks = character(),
##                                     open_first = TRUE,
##                                     open_last = FALSE),
##                  "'breaks' has length 0 but 'open_first' is TRUE")
## })
