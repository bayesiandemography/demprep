
context("infer_lab")


## Functions for individual "Label" classes -----------------------------------

test_that("infer_lab_categories gives correct answer with valid inputs", {
    infer_lab_categories <- demprep:::infer_lab_categories
    labels <- c("A", "B", "C")
    expect_identical(infer_lab_categories(labels),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- c("A", "B", NA, "C")
    expect_identical(infer_lab_categories(labels),
                     LabCategories(labels = labels[-3],
                                   include_na = TRUE))
    labels <- NA_character_
    expect_identical(infer_lab_categories(labels),
                     LabCategories(labels = character(),
                                   include_na = TRUE))
})

test_that("infer_lab_categories throws correct error with invalid inputs", {
    infer_lab_categories <- demprep:::infer_lab_categories
    labels <- c("A", "B", "")
    expect_error(infer_lab_categories(labels),
                 "'labels' has blanks")
})

test_that("infer_lab_triangles gives correct answer with valid inputs", {
    infer_lab_triangles <- demprep:::infer_lab_triangles
    labels <- c("Upper", "Lower", "Upper")
    expect_identical(infer_lab_triangles(labels),
                     LabTriangles(include_na = FALSE))
    labels <- c("Lower", "Upper", NA)
    expect_identical(infer_lab_triangles(labels),
                     LabTriangles(include_na = TRUE))
    labels <- NA_character_
    expect_identical(infer_lab_triangles(labels),
                     LabTriangles(include_na = TRUE))
})

test_that("infer_lab_triangles throws correct error or message with invalid inputs", {
    infer_lab_triangles <- demprep:::infer_lab_triangles
    labels <- ""
    expect_error(infer_lab_triangles(labels),
                 "'labels' has blanks")
    labels <- "wrong"
    expect_identical(infer_lab_triangles(labels),
                     "\"wrong\" not a valid label for triangles")
})

test_that("infer_lab_pool gives correct answer with valid inputs", {
    infer_lab_pool <- demprep:::infer_lab_pool
    labels <- c("Ins", "Outs", "Ins")
    expect_identical(infer_lab_pool(labels),
                     LabPool(include_na = FALSE))
    labels <- c("Outs", "Ins", NA)
    expect_identical(infer_lab_pool(labels),
                     LabPool(include_na = TRUE))
    labels <- NA_character_
    expect_identical(infer_lab_pool(labels),
                     LabPool(include_na = TRUE))
})

test_that("infer_lab_pool throws correct error or message with invalid inputs", {
    infer_lab_pool <- demprep:::infer_lab_pool
    labels <- ""
    expect_error(infer_lab_pool(labels),
                 "'labels' has blanks")
    labels <- "wrong"
    expect_identical(infer_lab_pool(labels),
                     "\"wrong\" not a valid label for pool")
})

test_that("infer_lab_quantiles gives correct answer with valid inputs", {
    infer_lab_quantiles <- demprep:::infer_lab_quantiles
    labels <- c("50.0001%", "50%", "1.254%")
    expect_identical(infer_lab_quantiles(labels),
                     LabQuantiles(labels = labels[c(3, 2, 1)],
                                  include_na = FALSE))
    labels <- c("0.001%", "100%", NA)
    expect_identical(infer_lab_quantiles(labels),
                     LabQuantiles(labels = labels[1:2],
                                  include_na = TRUE))
    labels <- NA_character_
    expect_identical(infer_lab_quantiles(labels),
                     LabQuantiles(labels = character(),
                                  include_na = TRUE))
})

test_that("infer_lab_quantiles throws correct error or message with invalid inputs", {
    infer_lab_quantiles <- demprep:::infer_lab_quantiles
    labels <- ""
    expect_error(infer_lab_quantiles(labels),
                 "'labels' has blanks")
    labels <- "wrong"
    expect_identical(infer_lab_quantiles(labels),
                     "\"wrong\" is not a valid quantile")
})

test_that("infer_lab_integers gives correct answer with valid inputs", {
    infer_lab_integers <- demprep:::infer_lab_integers
    labels <- c("1", "12", "4", "-1")
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = -1L,
                                 int_max = 12L,
                                 include_na = FALSE))
    labels <- c("0", "0", NA)
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = 0L,
                                 int_max = 0L,
                                 include_na = TRUE))
})

test_that("infer_lab_integers throws correct error or message with invalid inputs", {
    infer_lab_integers <- demprep:::infer_lab_integers
    labels <- ""
    expect_error(infer_lab_integers(labels),
                 "'labels' has blanks")
    labels <- c(NA_character_, NA_character_)
    expect_identical(infer_lab_integers(labels),
                     "'labels' has no non-NA elements")
})

test_that("infer_lab_grouped_int_enumeration gives correct answer with valid inputs", {
    infer_lab_grouped_int_enumerations <- demprep:::infer_lab_grouped_int_enumerations
    labels <- c("0", "1-4", "5-9", "10+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = c(0L, 1L, 5L, 10L),
                                               open_first = FALSE,
                                               open_last = TRUE,
                                               include_na = FALSE))
    labels <- c("10+", "0", "1-4", "5-9", "-5--1", NA, "<-5")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = c(-5L, 0L, 1L, 5L, 10L),
                                               open_first = TRUE,
                                               open_last = TRUE,
                                               include_na = TRUE))
    labels <- "<10"
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = 10L,
                                               open_first = TRUE,
                                               open_last = FALSE,
                                               include_na = FALSE))
    labels <- c(NA, "10+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = 10L,
                                               open_first = FALSE,
                                               open_last = TRUE,
                                               include_na = TRUE))
    labels <- c("10", "11", "8-9", NA, "12+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = c(8L, 10L, 11L, 12L),
                                               open_first = FALSE,
                                               open_last = TRUE,
                                               include_na = TRUE))
})

test_that("infer_lab_grouped_int_enumerations gives correct errors/messages with invalid inputs", {
    infer_lab_grouped_int_enumerations <- demprep:::infer_lab_grouped_int_enumerations
    labels <- c(NA, NA)
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "'labels' has no non-NA elements")
    labels <- c("wrong", NA)
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "\"wrong\" not a valid enumeration label")
    labels <- c("<10", "<5", "20-29")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "two different labels for interval open on left : \"<5\" and \"<10\"")
    labels <- c("100+", "50+", "20-29")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "two different labels for interval open on right : \"50+\" and \"100+\"")
    labels <- "10-10"
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "\"10-10\" not a valid enumeration label")
    labels <- "10-9"
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "\"10-9\" not a valid enumeration label")
    labels <- c("<10", "5+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"<10\" and \"5+\" overlap")
    labels <- c("<10", "10-14", "15-19", "20-25", "25-29")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"20-25\" and \"25-29\" overlap")
    labels <- c("0-4", "0", "1-4")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"0-4\" and \"0\" overlap")
    labels <- c("20+", "<25")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"<25\" and \"20+\" overlap")
    labels <- c("20-30", "<25")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"<25\" and \"20-30\" overlap")
    labels <- c("20-30", "25+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"20-30\" and \"25+\" overlap")
})

test_that("infer_lab_grouped_int_endpoints gives correct answer with valid inputs", {
    infer_lab_grouped_int_endpoints <- demprep:::infer_lab_grouped_int_endpoints
    labels <- c("0-1", "1-5", "5-10", "10+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = FALSE,
                                            open_last = TRUE,
                                            include_na = FALSE))
    labels <- c("10+", "0-1", "1-5", "5-10", "-5--0", NA, "<-5")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = c(-5L, 0L, 1L, 5L, 10L),
                                            open_first = TRUE,
                                            open_last = TRUE,
                                            include_na = TRUE))
    labels <- "<10"
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = 10L,
                                            open_first = TRUE,
                                            open_last = FALSE,
                                            include_na = FALSE))
    labels <- c(NA, "10+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = 10L,
                                            open_first = FALSE,
                                            open_last = TRUE,
                                            include_na = TRUE))
    labels <- c("10-11", "11-12", "8-10", NA, "12+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = c(8L, 10L, 11L, 12L),
                                            open_first = FALSE,
                                            open_last = TRUE,
                                            include_na = TRUE))
})

test_that("infer_lab_grouped_int_endpoints gives correct errors/messages with invalid inputs", {
    infer_lab_grouped_int_endpoints <- demprep:::infer_lab_grouped_int_endpoints
    labels <- c(NA, NA)
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "'labels' has no non-NA elements")
    labels <- c("wrong", NA)
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "\"wrong\" not a valid endpoints label")
    labels <- c("<10", "<5", "20-30")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "two different labels for interval open on left : \"<5\" and \"<10\"")
    labels <- c("100+", "50+", "20-30")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "two different labels for interval open on right : \"50+\" and \"100+\"")
    labels <- "10-10"
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "\"10-10\" not a valid endpoints label")
    labels <- "10-9"
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "\"10-9\" not a valid endpoints label")
    labels <- c("<10", "5+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"<10\" and \"5+\" overlap")
    labels <- c("<10", "10-15", "15-20", "20-25", "24-29")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"20-25\" and \"24-29\" overlap")
    labels <- c("0-5", "0-1", "1-5")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"0-5\" and \"0-1\" overlap")
    labels <- c("20+", "<25")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"<25\" and \"20+\" overlap")
    labels <- c("20-30", "<25")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"<25\" and \"20-30\" overlap")
    labels <- c("20-30", "25+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"20-30\" and \"25+\" overlap")
})

test_that("infer_lab_calendar_quarters gives correct answer with valid inputs", {
    infer_lab_calendar_quarters <- demprep:::infer_lab_calendar_quarters
    labels <- c("2019 Q3", "2019 Q1", "<2018 Q3", "2020 Q1+")
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2018-07-01"),
                                         break_max = as.Date("2020-01-01"),
                                         open_first = TRUE,
                                         open_last = TRUE,
                                         include_na = FALSE))
    labels <- "<2019 Q3"
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2019-07-01"),
                                         break_max = as.Date("2019-07-01"),
                                         open_first = TRUE,
                                         open_last = FALSE,
                                         include_na = FALSE))
    labels <- "2019 Q3+"
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2019-07-01"),
                                         break_max = as.Date("2019-07-01"),
                                         open_first = FALSE,
                                         open_last = TRUE,
                                         include_na = FALSE))
    labels <- c(NA, "<2019 Q3", "2019 Q3+")
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2019-07-01"),
                                         break_max = as.Date("2019-07-01"),
                                         open_first = TRUE,
                                         open_last = TRUE,
                                         include_na = TRUE))
    labels <- "2000 Q1"
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2000-01-01"),
                                         break_max = as.Date("2000-04-01"),
                                         open_first = FALSE,
                                         open_last = FALSE,
                                         include_na = FALSE))
})

test_that("infer_lab_calendar_quarters gives correct errors/messages with invalid inputs", {
    infer_lab_calendar_quarters <- demprep:::infer_lab_calendar_quarters
    labels <- c(NA, NA)
    expect_identical(infer_lab_calendar_quarters(labels),
                     "'labels' has no non-NA elements")
    labels <- c("wrong", NA)
    expect_identical(infer_lab_calendar_quarters(labels),
                     "\"wrong\" not a valid label for period of one quarter")
    labels <- c("<2000 Q1", "<2000 Q2", "2000 Q4")
    expect_identical(infer_lab_calendar_quarters(labels),
                     "two different labels for period open on left : \"<2000 Q1\" and \"<2000 Q2\"")
    labels <- c("2000 Q4+", "2001 Q3+", NA)
    expect_identical(infer_lab_calendar_quarters(labels),
                     "two different labels for period open on right : \"2000 Q4+\" and \"2001 Q3+\"")
    labels <- c("<2005 Q1", "2004 Q4+")
    expect_identical(infer_lab_calendar_quarters(labels),
                     "periods \"<2005 Q1\" and \"2004 Q4+\" overlap")
    labels <- c("2010 Q1", "2005 Q1", "<2006 Q3")
    expect_identical(infer_lab_calendar_quarters(labels),
                     "periods \"<2006 Q3\" and \"2005 Q1\" overlap")
    labels <- c("2010 Q1", "2005 Q1", "2006 Q3+")
    expect_identical(infer_lab_calendar_quarters(labels),
                     "periods \"2010 Q1\" and \"2006 Q3+\" overlap")
})

test_that("infer_lab_calendar_months gives correct answer with valid inputs", {
    infer_lab_calendar_months <- demprep:::infer_lab_calendar_months
    labels <- c("2019 Nov", "2019 Oct", "<2018 Feb", "2020 Mar+")
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2018-02-01"),
                                       break_max = as.Date("2020-03-01"),
                                       open_first = TRUE,
                                       open_last = TRUE,
                                       include_na = FALSE))
    labels <- "<2019 Sep"
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2019-09-01"),
                                       break_max = as.Date("2019-09-01"),
                                       open_first = TRUE,
                                       open_last = FALSE,
                                       include_na = FALSE))
    labels <- "2019 Sep+"
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2019-09-01"),
                                       break_max = as.Date("2019-09-01"),
                                       open_first = FALSE,
                                       open_last = TRUE,
                                       include_na = FALSE))
    labels <- c(NA, "<2019 Apr", "2019 Apr+")
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2019-04-01"),
                                       break_max = as.Date("2019-04-01"),
                                       open_first = TRUE,
                                       open_last = TRUE,
                                       include_na = TRUE))
    labels <- "2000 Feb"
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2000-02-01"),
                                       break_max = as.Date("2000-03-01"),
                                       open_first = FALSE,
                                       open_last = FALSE,
                                       include_na = FALSE))
})

test_that("infer_lab_calendar_months gives correct errors/messages with invalid inputs", {
    infer_lab_calendar_months <- demprep:::infer_lab_calendar_months
    labels <- c(NA, NA)
    expect_identical(infer_lab_calendar_months(labels),
                     "'labels' has no non-NA elements")
    labels <- c("wrong", NA)
    expect_identical(infer_lab_calendar_months(labels),
                     "\"wrong\" not a valid label for period of one month")
    labels <- c("<2000 Jan", "<2000 Feb", "2000 Mar")
    expect_identical(infer_lab_calendar_months(labels),
                     "two different labels for period open on left : \"<2000 Jan\" and \"<2000 Feb\"")
    labels <- c("2000 Dec+", "2000 Feb+", NA)
    expect_identical(infer_lab_calendar_months(labels),
                     "two different labels for period open on right : \"2000 Feb+\" and \"2000 Dec+\"")
    labels <- c("<2005 Feb", "2004 Dec+")
    expect_identical(infer_lab_calendar_months(labels),
                     "periods \"<2005 Feb\" and \"2004 Dec+\" overlap")
    labels <- c("2010 Jul", "2005 Jun", "<2006 Jul")
    expect_identical(infer_lab_calendar_months(labels),
                     "periods \"<2006 Jul\" and \"2005 Jun\" overlap")
    labels <- c("2010 Jan", "2005 Aug", "2006 Aug+")
    expect_identical(infer_lab_calendar_months(labels),
                     "periods \"2010 Jan\" and \"2006 Aug+\" overlap")
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
