
## Age ------------------------------------------------------------------------

test_that("'make_labels_age' gives correct answer with valid inputs", {
    expect_identical(make_labels_age(breaks = c(0L, 1L, 5L, 10L),
                                     open_last = TRUE,
                                     include_na = FALSE),
                     c("0", "1-4", "5-9", "10+"))
    expect_identical(make_labels_age(breaks = integer(),
                                     open_last = FALSE,
                                     include_na = FALSE),
                     character())
    expect_identical(make_labels_age(breaks = integer(),
                                     open_last = FALSE,
                                     include_na = TRUE),
                     NA_character_)
    expect_identical(make_labels_age(breaks = 2000L,
                                     open_last = TRUE,
                                     include_na = TRUE),
                     c("2000+", NA))
    expect_identical(make_labels_age(breaks = 2000L,
                                     open_last = TRUE,
                                     include_na = FALSE),
                     "2000+")
    expect_identical(make_labels_age(breaks = 1995:2000,
                                     open_last = FALSE,
                                     include_na = TRUE),
                     c(as.character(1995:1999), NA))
    expect_identical(make_labels_age(breaks = 1995:2000,
                                     open_last = TRUE,
                                     include_na = TRUE),
                     c(1995:1999, "2000+", NA))
    expect_identical(make_labels_age(breaks = c(1995L, 2000L),
                                     open_last = FALSE,
                                     include_na = FALSE),
                     "1995-1999")
})


## Period ---------------------------------------------------------------------

test_that("'make_labels_period_year' gives correct answer with valid inputs", {
    expect_identical(make_labels_period_year(break_min = 2000L,
                                             break_max = 2005L,
                                             include_na = TRUE),
                     c(as.character(2000:2004), NA))
    expect_identical(make_labels_period_year(break_min = 2000L,
                                             break_max = 2005L,
                                             include_na = FALSE),
                     as.character(2000:2004))
    expect_identical(make_labels_period_year(break_min = 2000L,
                                             break_max = 2001L,
                                             include_na = TRUE),
                     c("2000", NA))
})

test_that("'make_labels_period_custom' gives correct answer with valid inputs", {
    expect_identical(make_labels_period_custom(breaks = c(2000L, 2005L, 2015L, 2016L),
                                               include_na = TRUE),
                     c("2000-2005", "2005-2015", "2015-2016", NA))
    expect_identical(make_labels_period_custom(breaks = integer(),
                                               include_na = FALSE),
                     character())
    expect_identical(make_labels_period_custom(breaks = integer(),
                                               include_na = TRUE),
                     NA_character_)
})

test_that("'make_labels_period_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_period_quarter(break_min = as.Date("2020-10-01"),
                                                break_max = as.Date("2022-01-01"),
                                                include_na = TRUE),
                     c("2020 Q4", "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4", NA))
    expect_identical(make_labels_period_quarter(break_min = as.Date("2000-01-01"),
                                                break_max = as.Date("2000-04-01"),
                                                include_na = FALSE),
                     "2000 Q1")
    expect_identical(make_labels_period_quarter(break_min = as.Date("2000-01-01"),
                                                break_max = as.Date("2000-04-01"),
                                                include_na = TRUE),
                     c("2000 Q1", NA))
})

test_that("'make_labels_period_month' gives correct answer with valid inputs", {
    expect_identical(make_labels_period_month(break_min = as.Date("2020-10-01"),
                                              break_max = as.Date("2021-02-01"),
                                              include_na = TRUE),
                     c("2020 Oct", "2020 Nov", "2020 Dec", "2021 Jan", NA))
    expect_identical(make_labels_period_month(break_min = as.Date("2000-01-01"),
                                              break_max = as.Date("2000-02-01"),
                                              include_na = FALSE),
                     "2000 Jan")
    expect_identical(make_labels_period_month(break_min = as.Date("2000-01-01"),
                                              break_max = as.Date("2000-02-01"),
                                              include_na = TRUE),
                     c("2000 Jan", NA))
})


## Cohort ---------------------------------------------------------------------

test_that("'make_labels_cohort_year' gives correct answer with valid inputs", {
    expect_identical(make_labels_cohort_year(break_min = 2000L,
                                             break_max = 2005L,
                                             open_first = TRUE,
                                             include_na = TRUE),
                     c("<2000", 2000:2004, NA))
    expect_identical(make_labels_cohort_year(break_min = 2000L,
                                             break_max = 2005L,
                                             open_first = FALSE,
                                             include_na = TRUE),
                     c(as.character(2000:2004), NA))
    expect_identical(make_labels_cohort_year(break_min = 2000L,
                                             break_max = 2005L,
                                             open_first = FALSE,
                                             include_na = FALSE),
                     as.character(2000:2004))
    expect_identical(make_labels_cohort_year(break_min = 2000L,
                                             break_max = 2001L,
                                             open_first = FALSE,
                                             include_na = TRUE),
                     c("2000", NA))
    expect_identical(make_labels_cohort_year(break_min = 2000L,
                                             break_max = 2000L,
                                             open_first = TRUE,
                                             include_na = FALSE),
                     "<2000")
})

test_that("'make_labels_cohort_custom' gives correct answer with valid inputs", {
    expect_identical(make_labels_cohort_custom(breaks = c(2000L, 2005L, 2015L, 2016L),
                                               open_first = TRUE,
                                               include_na = TRUE),
                     c("<2000", "2000-2005", "2005-2015", "2015-2016", NA))
    expect_identical(make_labels_cohort_custom(breaks = integer(),
                                               open_first = FALSE,
                                               include_na = FALSE),
                     character())
    expect_identical(make_labels_cohort_custom(breaks = 2000L,
                                               open_first = TRUE,
                                               include_na = FALSE),
                     "<2000")
    expect_identical(make_labels_cohort_custom(breaks = integer(),
                                               open_first = FALSE,
                                               include_na = TRUE),
                     NA_character_)
})

test_that("'make_labels_cohort_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_cohort_quarter(break_min = as.Date("2020-10-01"),
                                                break_max = as.Date("2022-01-01"),
                                                open_first = FALSE,
                                                include_na = TRUE),
                     c("2020 Q4", "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4", NA))
    expect_identical(make_labels_cohort_quarter(break_min = as.Date("2000-01-01"),
                                                break_max = as.Date("2000-04-01"),
                                                open_first = TRUE,
                                                include_na = FALSE),
                     c("<2000 Q1", "2000 Q1"))
    expect_identical(make_labels_cohort_quarter(break_min = as.Date("2000-01-01"),
                                                break_max = as.Date("2000-04-01"),
                                                open_first = FALSE,
                                                include_na = TRUE),
                     c("2000 Q1", NA))
})

test_that("'make_labels_cohort_month' gives correct answer with valid inputs", {
    expect_identical(make_labels_cohort_month(break_min = as.Date("2020-10-01"),
                                              break_max = as.Date("2021-02-01"),
                                              open_first = TRUE,
                                              include_na = TRUE),
                     c("<2020 Oct", "2020 Oct", "2020 Nov", "2020 Dec", "2021 Jan", NA))
    expect_identical(make_labels_cohort_month(break_min = as.Date("2000-01-01"),
                                              break_max = as.Date("2000-02-01"),
                                              open_first = FALSE,
                                              include_na = FALSE),
                     "2000 Jan")
    expect_identical(make_labels_cohort_month(break_min = as.Date("2000-01-01"),
                                              break_max = as.Date("2000-02-01"),
                                              open_first = TRUE,
                                              include_na = TRUE),
                     c("<2000 Jan", "2000 Jan", NA))
})
