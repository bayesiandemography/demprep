
context("infer_dimscale")

## AgeGroup

test_that("can create object of class AgeGroupSingle", {
    x <- new("AgeGroupSingle",
             break_min = 0L,
             break_max = 100L,
             open_last = TRUE,
             include_na = FALSE)
    expect_true(validObject(x))
})

test_that("can create object of class AgeGroupMulti", {
    x <- new("AgeGroupMulti",
             break_min = 0L,
             break_max = 100L,
             width = 5L,
             open_last = TRUE,
             include_na = FALSE)
    expect_true(validObject(x))
})

test_that("invalid values for AgeGroupMulti raise appropriate error", {
    expect_error(new("AgeGroupMulti",
                     break_min = 0L,
                     break_max = 100L,
                     width = 6L,
                     open_last = TRUE,
                     include_na = FALSE),
                 paste("difference between 'break_max' \\[100\\] and 'break_min' \\[0\\]",
                       "not a multiple of 'width' \\[6\\]"))
})

test_that("can create object of class AgeGroupCustom", {
    x <- new("AgeGroupCustom",
             breaks = c(0L, 1L, 5L, 10L, 20L),
             open_last = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("AgeGroupCustom",
             breaks = 0L,
             open_last = TRUE,
             include_na = FALSE)
    expect_true(validObject(x))
    x <- new("AgeGroupCustom",
             breaks = integer(),
             open_last = FALSE,
             include_na = FALSE)
    expect_true(validObject(x))
    x <- new("AgeGroupCustom",
             breaks = integer(),
             open_last = FALSE,
             include_na = TRUE)
    expect_true(validObject(x))
})

test_that("can create object of class AgeGroupQuarter", {
    x <- new("AgeGroupQuarter",
             break_min = 4L,
             break_max = 10L,
             open_last = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("AgeGroupQuarter",
             break_min = 0L,
             break_max = 1L,
             open_last = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
})

test_that("can create object of class AgeGroupMonth", {
    x <- new("AgeGroupMonth",
             break_min = 4L,
             break_max = 10L,
             open_last = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("AgeGroupMonth",
             break_min = 0L,
             break_max = 1L,
             open_last = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
})


## Period

test_that("can create object of class PeriodSingle", {
    x <- new("PeriodSingle",
             year_min = 2000L,
             year_max = 2010L,
             include_na = FALSE)
    expect_true(validObject(x))
})

test_that("can create object of class PeriodMulti", {
    x <- new("PeriodMulti",
             year_min = 2000L,
             year_max = 2100L,
             width = 5L,
             include_na = FALSE)
    expect_true(validObject(x))
})

test_that("invalid values for PeriodMulti raise appropriate error", {
    expect_error(new("PeriodMulti",
                     year_min = 2000L,
                     year_max = 2100L,
                     width = 6L,
                     include_na = FALSE),
                 paste("difference between 'year_max' \\[2100\\] and 'year_min' \\[2000\\]",
                       "not a multiple of 'width' \\[6\\]"))
})

test_that("can create object of class PeriodCustom", {
    x <- new("PeriodCustom",
             years = c(2000L, 2001L, 2005L, 2010L, 2020L),
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("PeriodCustom",
             years = integer(),
             include_na = FALSE)
    expect_true(validObject(x))
    x <- new("PeriodCustom",
             years = integer(),
             include_na = TRUE)
    expect_true(validObject(x))
})

test_that("can create object of class PeriodQuarter", {
    x <- new("PeriodQuarter",
             break_min = as.Date("2000-04-01"),
             break_max = as.Date("2010-01-01"),
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("PeriodQuarter",
             break_min = as.Date("2019-10-01"),
             break_max = as.Date("2020-01-01"),
             include_na = TRUE)
    expect_true(validObject(x))
})

test_that("can create object of class PeriodMonth", {
    x <- new("PeriodMonth",
             break_min = as.Date("2000-04-01"),
             break_max = as.Date("2010-02-01"),
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("PeriodMonth",
             break_min = as.Date("2000-01-01"),
             break_max = as.Date("2000-02-01"),
             include_na = TRUE)
    expect_true(validObject(x))
})


## Cohort

test_that("can create object of class CohortSingle", {
    x <- new("CohortSingle",
             year_min = 2000L,
             year_max = 2010L,
             open_first = FALSE,
             include_na = FALSE)
    expect_true(validObject(x))
})

test_that("can create object of class CohortMulti", {
    x <- new("CohortMulti",
             year_min = 2000L,
             year_max = 2100L,
             width = 5L,
             open_first = TRUE,
             include_na = FALSE)
    expect_true(validObject(x))
})

test_that("invalid values for CohortMulti raise appropriate error", {
    expect_error(new("CohortMulti",
                     year_min = 2000L,
                     year_max = 2100L,
                     width = 6L,
                     open_first = TRUE,
                     include_na = FALSE),
                 paste("difference between 'year_max' \\[2100\\] and 'year_min' \\[2000\\]",
                       "not a multiple of 'width' \\[6\\]"))
})

test_that("can create object of class CohortCustom", {
    x <- new("CohortCustom",
             years = c(2000L, 2001L, 2005L, 2010L, 2020L),
             open_first = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("CohortCustom",
             years = 2000L,
             open_first = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("CohortCustom",
             years = integer(),
             open_first = FALSE,
             include_na = FALSE)
    expect_true(validObject(x))
    x <- new("CohortCustom",
             years = integer(),
             open_first = FALSE,
             include_na = TRUE)
    expect_true(validObject(x))
})

test_that("can create object of class CohortQuarter", {
    x <- new("CohortQuarter",
             break_min = as.Date("2000-04-01"),
             break_max = as.Date("2010-01-01"),
             open_first = FALSE,
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("CohortQuarter",
             break_min = as.Date("2019-10-01"),
             break_max = as.Date("2020-01-01"),
             open_first = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
})

test_that("can create object of class CohortMonth", {
    x <- new("CohortMonth",
             break_min = as.Date("2000-04-01"),
             break_max = as.Date("2010-02-01"),
             open_first = TRUE,
             include_na = TRUE)
    expect_true(validObject(x))
    x <- new("CohortMonth",
             break_min = as.Date("2000-01-01"),
             break_max = as.Date("2000-02-01"),
             open_first = FALSE,
             include_na = TRUE)
    expect_true(validObject(x))
})





## ## infer_dimscale_period_quarter ----------------------------------------------

## test_that("infer_dimscale_period_quarter gives correct answer with multiple quarters, and all options TRUE", {
##     break_min <- as.Date("2000-01-01")
##     break_max <- as.Date("2001-04-01")
##     open_first <- TRUE
##     open_last <- TRUE
##     include_na <- TRUE
##     labels <- make_labels_period_quarter(break_min = break_min,
##                                          break_max = break_max,
##                                          open_first = open_first,
##                                          open_last = open_last,
##                                          include_na = include_na)
##     ans_obtained <- infer_dimscale_period_quarter(labels)
##     ans_expected <- list(break_min = break_min,
##                          break_max = break_max,
##                          open_first = open_first,
##                          open_last = open_last,
##                          include_na = include_na,
##                          has_gap = FALSE)
##     expect_identical(ans_obtained, ans_expected)
## })

## test_that("infer_dimscale_period_quarter gives correct answer with out-of-order labels", {
##     break_min <- as.Date("2000-01-01")
##     break_max <- as.Date("2001-04-01")
##     open_first <- TRUE
##     open_last <- TRUE
##     include_na <- TRUE
##     labels <- make_labels_period_quarter(break_min = break_min,
##                                        break_max = break_max,
##                                        open_first = open_first,
##                                        open_last = open_last,
##                                        include_na = include_na)
##     labels <- rev(rep(labels, each = 2))
##     ans_obtained <- infer_dimscale_period_quarter(labels)
##     ans_expected <- list(break_min = break_min,
##                          break_max = break_max,
##                          open_first = open_first,
##                          open_last = open_last,
##                          include_na = include_na,
##                          has_gap = FALSE)
##     expect_identical(ans_obtained, ans_expected)
## })

## test_that("infer_dimscale_period_quarter gives correct answer with single quarter", {
##     break_min <- as.Date("2000-01-01")
##     break_max <- as.Date("2000-04-01")
##     open_first <- FALSE
##     open_last <- FALSE
##     include_na <- FALSE
##     labels <- make_labels_period_quarter(break_min = break_min,
##                                        break_max = break_max,
##                                        open_first = open_first,
##                                        open_last = open_last,
##                                        include_na = include_na)
##     ans_obtained <- infer_dimscale_period_quarter(labels)
##     ans_expected <- list(break_min = break_min,
##                          break_max = break_max,
##                          open_first = open_first,
##                          open_last = open_last,
##                          include_na = include_na,
##                          has_gap = FALSE)
##     expect_identical(ans_obtained, ans_expected)
## })

## test_that("infer_dimscale_period_quarter gives correct answer when gaps_ok is TRUE", {
##     break_min <- as.Date("2000-01-01")
##     break_max <- as.Date("2001-04-01")
##     open_first <- TRUE
##     open_last <- TRUE
##     include_na <- TRUE
##     gaps_ok <- TRUE
##     labels <- make_labels_period_quarter(break_min = break_min,
##                                        break_max = break_max,
##                                        open_first = open_first,
##                                        open_last = open_last,
##                                        include_na = include_na)
##     labels <- labels[-3L]
##     ans_obtained <- infer_dimscale_period_quarter(labels, gaps_ok = TRUE)
##     ans_expected <- list(break_min = break_min,
##                          break_max = break_max,
##                          open_first = open_first,
##                          open_last = open_last,
##                          include_na = include_na,
##                          has_gap = TRUE)
##     expect_identical(ans_obtained, ans_expected)
## })

## test_that("infer_dimscale_period_quarter gives correct message with invalid input", {
##     expect_identical(infer_dimscale_period_quarter("wrong"),
##                      "\"wrong\" not a valid label for period of one quarter")
##     expect_identical(infer_dimscale_period_quarter("2000 q2+"),
##                      "\"2000 q2+\" not a valid label for period of one quarter")
##     expect_identical(infer_dimscale_period_quarter(c("<2000 Q1", "1999 Q4")),
##                      "label \"<2000 Q1\" is open on the left, but label \"1999 Q4\" refers to an earlier date")
##     expect_identical(infer_dimscale_period_quarter(c("2000 Q4", NA, "2000 Q1+")),
##                      "label \"2000 Q1+\" is open on the right, but label \"2000 Q4\" refers to a later date")
##     expect_identical(infer_dimscale_period_quarter(c("2000 Q4", NA, "2001 Q2", "2000 Q3", "2001 Q3")),
##                      "period labels have a gap : no label for period \"2001 Q1\"")
## })









## ## infer_dimscale_period_month ------------------------------------------------

## test_that("infer_dimscale_period_month gives correct answer with multiple months, and all options TRUE", {
##     break_min <- as.Date("2000-01-01")
##     break_max <- as.Date("2001-04-01")
##     open_first <- TRUE
##     open_last <- TRUE
##     include_na <- TRUE
##     labels <- make_labels_period_month(break_min = break_min,
##                                        break_max = break_max,
##                                        open_first = open_first,
##                                        open_last = open_last,
##                                        include_na = include_na)
##     ans_obtained <- infer_dimscale_period_month(labels)
##     ans_expected <- list(break_min = break_min,
##                          break_max = break_max,
##                          open_first = open_first,
##                          open_last = open_last,
##                          include_na = include_na,
##                          has_gap = FALSE)
##     expect_identical(ans_obtained, ans_expected)
## })

## test_that("infer_dimscale_period_month gives correct answer with out-of-order labels", {
##     break_min <- as.Date("2000-01-01")
##     break_max <- as.Date("2001-04-01")
##     open_first <- TRUE
##     open_last <- TRUE
##     include_na <- TRUE
##     labels <- make_labels_period_month(break_min = break_min,
##                                        break_max = break_max,
##                                        open_first = open_first,
##                                        open_last = open_last,
##                                        include_na = include_na)
##     labels <- rev(rep(labels, each = 2))
##     ans_obtained <- infer_dimscale_period_month(labels)
##     ans_expected <- list(break_min = break_min,
##                          break_max = break_max,
##                          open_first = open_first,
##                          open_last = open_last,
##                          include_na = include_na,
##                          has_gap = FALSE)
##     expect_identical(ans_obtained, ans_expected)
## })

## test_that("infer_dimscale_period_month gives correct answer with single month", {
##     break_min <- as.Date("2000-01-01")
##     break_max <- as.Date("2000-02-01")
##     open_first <- FALSE
##     open_last <- FALSE
##     include_na <- FALSE
##     labels <- make_labels_period_month(break_min = break_min,
##                                        break_max = break_max,
##                                        open_first = open_first,
##                                        open_last = open_last,
##                                        include_na = include_na)
##     ans_obtained <- infer_dimscale_period_month(labels)
##     ans_expected <- list(break_min = break_min,
##                          break_max = break_max,
##                          open_first = open_first,
##                          open_last = open_last,
##                          include_na = include_na,
##                          has_gap = FALSE)
##     expect_identical(ans_obtained, ans_expected)
## })

## test_that("infer_dimscale_period_month gives correct answer when gaps_ok is TRUE", {
##     break_min <- as.Date("2000-01-01")
##     break_max <- as.Date("2001-04-01")
##     open_first <- TRUE
##     open_last <- TRUE
##     include_na <- TRUE
##     gaps_ok <- TRUE
##     labels <- make_labels_period_month(break_min = break_min,
##                                        break_max = break_max,
##                                        open_first = open_first,
##                                        open_last = open_last,
##                                        include_na = include_na)
##     labels <- labels[-3L]
##     ans_obtained <- infer_dimscale_period_month(labels, gaps_ok = TRUE)
##     ans_expected <- list(break_min = break_min,
##                          break_max = break_max,
##                          open_first = open_first,
##                          open_last = open_last,
##                          include_na = include_na,
##                          has_gap = TRUE)
##     expect_identical(ans_obtained, ans_expected)
## })

## test_that("infer_dimscale_period_month gives correct message with invalid input", {
##     expect_identical(infer_dimscale_period_month("wrong"),
##                      "\"wrong\" not a valid label for period of one month")
##     expect_identical(infer_dimscale_period_month("2000 Bad+"),
##                      "\"2000 Bad+\" not a valid label for period of one month")
##     expect_identical(infer_dimscale_period_month(c("<2000 Jan", "1999 Dec")),
##                      "label \"<2000 Jan\" is open on the left, but label \"1999 Dec\" refers to an earlier date")
##     expect_identical(infer_dimscale_period_month(c("2000 Dec", NA, "2000 Jan+")),
##                      "label \"2000 Jan+\" is open on the right, but label \"2000 Dec\" refers to a later date")
##     expect_identical(infer_dimscale_period_month(c("2000 Dec", NA, "2001 Jan", "2000 Nov", "2001 Mar")),
##                      "period labels have a gap : no label for period \"2001 Feb\"")
## })
