
context("infer_dimscale")

## infer_dimscale_period_month ------------------------------------------------

test_that("infer_dimscale_period_month gives correct answer with multiple months, and all options TRUE", {
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
                         include_na = include_na,
                         has_gap = FALSE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("infer_dimscale_period_month gives correct answer with out-of-order labels", {
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
    labels <- rev(rep(labels, each = 2))
    ans_obtained <- infer_dimscale_period_month(labels)
    ans_expected <- list(break_min = break_min,
                         break_max = break_max,
                         open_first = open_first,
                         open_last = open_last,
                         include_na = include_na,
                         has_gap = FALSE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("infer_dimscale_period_month gives correct answer with single month", {
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
                         include_na = include_na,
                         has_gap = FALSE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("infer_dimscale_period_month gives correct answer when gaps_ok is TRUE", {
    break_min <- as.Date("2000-01-01")
    break_max <- as.Date("2001-04-01")
    open_first <- TRUE
    open_last <- TRUE
    include_na <- TRUE
    gaps_ok <- TRUE
    labels <- make_labels_period_month(break_min = break_min,
                                       break_max = break_max,
                                       open_first = open_first,
                                       open_last = open_last,
                                       include_na = include_na)
    labels <- labels[-3L]
    ans_obtained <- infer_dimscale_period_month(labels, gaps_ok = TRUE)
    ans_expected <- list(break_min = break_min,
                         break_max = break_max,
                         open_first = open_first,
                         open_last = open_last,
                         include_na = include_na,
                         has_gap = TRUE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("infer_dimscale_period_month gives correct message with invalid input", {
    expect_identical(infer_dimscale_period_month("wrong"),
                     "\"wrong\" is not a valid label for a period of one month")
    expect_identical(infer_dimscale_period_month("2000 Bad+"),
                     "\"2000 Bad+\" is not a valid label for a period of one month")
    expect_identical(infer_dimscale_period_month(c("<2000 Jan", "1999 Dec")),
                     "label \"<2000 Jan\" is open on the left, but label \"1999 Dec\" refers to an earlier date")
    expect_identical(infer_dimscale_period_month(c("2000 Dec", NA, "2000 Jan+")),
                     "label \"2000 Jan+\" is open on the right, but label \"2000 Dec\" refers to a later date")
    expect_identical(infer_dimscale_period_month(c("2000 Dec", NA, "2001 Jan", "2000 Nov", "2001 Mar")),
                     "period labels have a gap : no label for period \"2001 Feb\"")
})
