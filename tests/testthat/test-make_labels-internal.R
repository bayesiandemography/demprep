
context("make_labels-internal")


## make_labels_complete_calendar_month ----------------------------------------

test_that("'make_labels_complete_calendar_month' gives correct answer with valid inputs", {
    expect_identical(make_labels_complete_calendar_month(break_min = as.Date("2019-01-01"),
                                                         break_max = as.Date("2019-04-01"),
                                                         open_first = FALSE,
                                                         include_na = TRUE),
                     c("2019 Jan", "2019 Feb", "2019 Mar", NA))
    expect_identical(make_labels_complete_calendar_month(break_min = as.Date("2019-10-01"),
                                                         break_max = as.Date("2020-01-01"),
                                                         open_first = FALSE,
                                                         include_na = FALSE),
                     c("2019 Oct", "2019 Nov", "2019 Dec"))
    expect_identical(make_labels_complete_calendar_month(break_min = as.Date("2019-10-01"),
                                                         break_max = as.Date("2020-01-01"),
                                                         open_first = TRUE,
                                                         include_na = FALSE),
                     c("<2019 Oct", "2019 Oct", "2019 Nov", "2019 Dec"))
    expect_identical(make_labels_complete_calendar_month(break_min = as.Date("2019-07-01"),
                                                         break_max = as.Date("2019-07-01"),
                                                         open_first = TRUE,
                                                         include_na = FALSE),
                     "<2019 Jul")
})

## make_labels_complete_calendar_quarter --------------------------------------

test_that("'make_labels_complete_calendar_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_complete_calendar_quarter(break_min = as.Date("2019-01-01"),
                                                           break_max = as.Date("2019-04-01"),
                                                           open_first = FALSE,
                                                           include_na = FALSE),
                     "2019 Q1")
    expect_identical(make_labels_complete_calendar_quarter(break_min = as.Date("2019-01-01"),
                                                           break_max = as.Date("2020-01-01"),
                                                           open_first = FALSE,
                                                           include_na = FALSE),
                     c("2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4"))
    expect_identical(make_labels_complete_calendar_quarter(break_min = as.Date("2019-07-01"),
                                                           break_max = as.Date("2020-07-01"),
                                                           open_first = TRUE,
                                                           include_na = TRUE),
                     c("<2019 Q3", "2019 Q3", "2019 Q4", "2020 Q1", "2020 Q2", NA))
    expect_identical(make_labels_complete_calendar_quarter(break_min = as.Date("2019-07-01"),
                                                           break_max = as.Date("2019-07-01"),
                                                           open_first = TRUE,
                                                           include_na = TRUE),
                     c("<2019 Q3", NA))
})

## make_labels_default --------------------------------------------------------

test_that("'make_labels_default' gives correct answer with valid inputs", {
    make_labels_default <- demprep:::make_labels_default
    lab <- c("A", "B", "C")
    expect_identical(make_labels_default(labels = lab,
                                         include_na = FALSE),
                     lab)
    expect_identical(make_labels_default(labels = character(),
                                         include_na = TRUE),
                     NA_character_)
})


## make_labels_integers --------------------------------------------------------

test_that("'make_labels_integers' gives correct answer with valid inputs", {
    make_labels_integers <- demprep:::make_labels_integers
    expect_identical(make_labels_integers(int_min = 0L,
                                          int_max = 4L,
                                          include_na = FALSE),
                     as.character(0:4))
    expect_identical(make_labels_integers(int_min = -1L,
                                          int_max = 0L,
                                          include_na = FALSE),
                     as.character(c(-1, 0)))
    expect_identical(make_labels_integers(int_min = 2000L,
                                          int_max = 2000L,
                                          include_na = TRUE),
                     c(2000, NA_character_))
})


## make_labels_grouped_int_enumerations ---------------------------------------

test_that("'make_labels_grouped_int_enumerations' gives correct answer with valid inputs", {
    make_labels_grouped_int_enumerations <- demprep:::make_labels_grouped_int_enumerations
    expect_identical(make_labels_grouped_int_enumerations(breaks = c(0, 1, 5, 10),
                                                          open_first = FALSE,
                                                          open_last = TRUE,
                                                          include_na = FALSE),
                     c("0", "1-4", "5-9", "10+"))
    expect_identical(make_labels_grouped_int_enumerations(breaks = integer(),
                                                          open_first = FALSE,
                                                          open_last = FALSE,
                                                          include_na = FALSE),
                     character())
    expect_identical(make_labels_grouped_int_enumerations(breaks = integer(),
                                                          open_first = FALSE,
                                                          open_last = FALSE,
                                                          include_na = TRUE),
                     NA_character_)
    expect_identical(make_labels_grouped_int_enumerations(breaks = 2000,
                                                          open_first = TRUE,
                                                          open_last = TRUE,
                                                          include_na = TRUE),
                     c("<2000", "2000+", NA))
    expect_identical(make_labels_grouped_int_enumerations(breaks = 2000,
                                                          open_first = FALSE,
                                                          open_last = TRUE,
                                                          include_na = FALSE),
                     "2000+")
    expect_identical(make_labels_grouped_int_enumerations(breaks = 1995:2000,
                                                          open_first = FALSE,
                                                          open_last = FALSE,
                                                          include_na = TRUE),
                     c(as.character(1995:1999), NA))
    expect_identical(make_labels_grouped_int_enumerations(breaks = 1995:2000,
                                                          open_first = FALSE,
                                                          open_last = TRUE,
                                                          include_na = TRUE),
                     c(1995:1999, "2000+", NA))
    expect_identical(make_labels_grouped_int_enumerations(breaks = c(1995, 2000),
                                                          open_first = TRUE,
                                                          open_last = FALSE,
                                                          include_na = FALSE),
                     c("<1995", "1995-1999"))
    expect_identical(make_labels_grouped_int_enumerations(breaks = (-1):1,
                                                          open_first = FALSE,
                                                          open_last = FALSE,
                                                          include_na = FALSE),
                     c("-1", "0"))
})
    
    
## make_labels_grouped_int_endpoints ---------------------------------------

test_that("'make_labels_grouped_int_endpoints' gives correct answer with valid inputs", {
    make_labels_grouped_int_endpoints <- demprep:::make_labels_grouped_int_endpoints
    expect_identical(make_labels_grouped_int_endpoints(breaks = c(0, 1, 5, 10),
                                                       open_first = FALSE,
                                                       open_last = FALSE,
                                                       include_na = FALSE),
                     c("0-1", "1-5", "5-10"))
    expect_identical(make_labels_grouped_int_endpoints(breaks = c(0, 1, 5, 10),
                                                       open_first = FALSE,
                                                       open_last = TRUE,
                                                       include_na = FALSE),
                     c("0-1", "1-5", "5-10", "10+"))
    expect_identical(make_labels_grouped_int_endpoints(breaks = integer(),
                                                       open_first = FALSE,
                                                       open_last = FALSE,
                                                       include_na = FALSE),
                     character())
    expect_identical(make_labels_grouped_int_endpoints(breaks = integer(),
                                                       open_first = FALSE,
                                                       open_last = FALSE,
                                                       include_na = TRUE),
                     NA_character_)
    expect_identical(make_labels_grouped_int_endpoints(breaks = 2000,
                                                       open_first = TRUE,
                                                       open_last = FALSE,
                                                       include_na = TRUE),
                     c("<2000", NA))
    expect_identical(make_labels_grouped_int_endpoints(breaks = 2000,
                                                       open_first = FALSE,
                                                       open_last = TRUE,
                                                       include_na = FALSE),
                     "2000+")
    expect_identical(make_labels_grouped_int_endpoints(breaks = 1995:1997,
                                                       open_first = FALSE,
                                                       open_last = FALSE,
                                                       include_na = TRUE),
                     c("1995-1996", "1996-1997", NA))
    expect_identical(make_labels_grouped_int_endpoints(breaks = 1995:1997,
                                                       open_first = FALSE,
                                                       open_last = FALSE,
                                                       include_na = TRUE),
                     c("1995-1996", "1996-1997", NA))
    expect_identical(make_labels_grouped_int_endpoints(breaks = c(1995, 2000),
                                                       open_first = TRUE,
                                                       open_last = FALSE,
                                                       include_na = FALSE),
                     c("<1995", "1995-2000"))
    expect_identical(make_labels_grouped_int_endpoints(breaks = (-1):1,
                                                       open_first = FALSE,
                                                       open_last = FALSE,
                                                       include_na = FALSE),
                     c("-1-0", "0-1"))
})    
    

## make_labels_calendar_quarters_months ----------------------------------------

test_that("'make_labels_calendar_quarters_months' gives correct answer with valid inputs", {
    make_labels_calendar_quarters_months <- demprep:::make_labels_calendar_quarters_months
    expect_identical(make_labels_calendar_quarters_months(break_min = as.Date("2019-01-01"),
                                                          break_max = as.Date("2019-04-01"),
                                                          open_first = FALSE,
                                                          open_last = FALSE,
                                                          unit = "month",
                                                          include_na = FALSE),
                     c("2019 Jan", "2019 Feb", "2019 Mar"))
    expect_identical(make_labels_calendar_quarters_months(break_min = as.Date("2019-01-01"),
                                                          break_max = as.Date("2019-04-01"),
                                                          open_first = FALSE,
                                                          open_last = TRUE,
                                                          unit = "month",
                                                          include_na = FALSE),
                     c("2019 Jan", "2019 Feb", "2019 Mar", "2019 Apr+"))
    expect_identical(make_labels_calendar_quarters_months(break_min = as.Date("2019-01-01"),
                                                          break_max = as.Date("2020-01-01"),
                                                          open_first = FALSE,
                                                          open_last = FALSE,
                                                          unit = "quarter",
                                                          include_na = FALSE),
                     c("2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4"))
    expect_identical(make_labels_calendar_quarters_months(break_min = as.Date("2019-07-01"),
                                                          break_max = as.Date("2020-07-01"),
                                                          open_first = TRUE,
                                                          open_last = FALSE,
                                                          unit = "quarter",
                                                          include_na = TRUE),
                     c("<2019 Q3", "2019 Q3", "2019 Q4", "2020 Q1", "2020 Q2", NA))
    expect_identical(make_labels_calendar_quarters_months(break_min = as.Date("2019-07-01"),
                                                          break_max = as.Date("2019-07-01"),
                                                          open_first = TRUE,
                                                          open_last = FALSE,
                                                          unit = "quarter",
                                                          include_na = FALSE),
                     "<2019 Q3")
    expect_identical(make_labels_calendar_quarters_months(break_min = as.Date("2019-07-01"),
                                                          break_max = as.Date("2019-07-01"),
                                                          open_first = TRUE,
                                                          open_last = TRUE,
                                                          unit = "quarter",
                                                          include_na = FALSE),
                     c("<2019 Q3", "2019 Q3+"))
})


## make_labels_duration_quarters_months ---------------------------------------

test_that("'make_labels_duration_quarters_months' gives correct answer with valid inputs", {
    expect_identical(make_labels_duration_quarters_months(break_min = 0,
                                                          break_max = 5,
                                                          open_last = FALSE,
                                                          include_na = FALSE),
                     c("0", "1", "2", "3", "4"))
    expect_identical(make_labels_duration_quarters_months(break_min = 0,
                                                          break_max = 5,
                                                          open_last = FALSE,
                                                          include_na = FALSE),
                     c("0", "1", "2", "3", "4"))
    expect_identical(make_labels_duration_quarters_months(break_min = 0,
                                                          break_max = 5,
                                                          open_last = TRUE,
                                                          include_na = FALSE),
                     c("0", "1", "2", "3", "4", "5+"))
    expect_identical(make_labels_duration_quarters_months(break_min = 0,
                                                          break_max = 5,
                                                          open_last = TRUE,
                                                          include_na = TRUE),
                     c("0", "1", "2", "3", "4", "5+", NA))
    expect_identical(make_labels_duration_quarters_months(break_min = 0,
                                                          break_max = 0,
                                                          open_last = TRUE,
                                                          include_na = FALSE),
                     "0+")
    expect_identical(make_labels_duration_quarters_months(break_min = 0,
                                                          break_max = 0,
                                                          open_last = TRUE,
                                                          include_na = TRUE),
                     c("0+", NA))
})



