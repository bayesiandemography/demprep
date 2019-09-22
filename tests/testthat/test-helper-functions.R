
context("helper-functions")

## age_completed_months ------------------------------------------------------

test_that("'age_completed_months' gives correct answer with valid inputs", {
    expect_identical(age_completed_months(date = as.Date("2000-01-01"),
                                          dob = as.Date("2000-01-01")),
                     0L)
    expect_identical(age_completed_months(date = as.Date("2000-01-31"),
                                          dob = as.Date("2000-01-01")),
                     0L)
    expect_identical(age_completed_months(date = as.Date("2000-02-01"),
                                          dob = as.Date("2000-01-01")),
                     1L)
    expect_identical(age_completed_months(date = as.Date("2000-02-28"),
                                          dob = as.Date("2000-01-01")),
                     1L)
    expect_identical(age_completed_months(date = as.Date("2000-02-29"),
                                          dob = as.Date("2000-01-01")),
                     1L)
    expect_identical(age_completed_months(date = as.Date("2000-03-01"),
                                          dob = as.Date("2000-01-01")),
                     2L)
    expect_identical(age_completed_months(date = as.Date("2000-12-31"),
                                          dob = as.Date("2000-01-01")),
                     11L)
    expect_identical(age_completed_months(date = as.Date("2001-01-01"),
                                          dob = as.Date("2000-01-01")),
                     12L)
    expect_identical(age_completed_months(date = as.Date("2001-02-28"),
                                          dob = as.Date("2000-01-01")),
                     13L)
    expect_identical(age_completed_months(date = as.Date("2001-03-01"),
                                          dob = as.Date("2000-01-01")),
                     14L)
    expect_identical(age_completed_months(date = as.Date(NA_character_),
                                          dob = as.Date("2000-01-01")),
                     NA_integer_)
    expect_identical(age_completed_months(date = as.Date("2000-03-27"),
                                          dob = as.Date("2000-02-28")),
                     0L)
    expect_identical(age_completed_months(date = as.Date("2000-03-28"),
                                          dob = as.Date("2000-02-28")),
                     1L)
    expect_identical(age_completed_months(date = as.Date("2000-03-27"),
                                          dob = as.Date("2000-02-29")),
                     0L)
    expect_identical(age_completed_months(date = as.Date("2000-03-28"),
                                          dob = as.Date("2000-02-29")),
                     1L)
    expect_identical(age_completed_months(date = as.Date("2000-03-29"),
                                          dob = as.Date("2000-02-29")),
                     1L)
    expect_identical(age_completed_months(date = as.Date(c("2000-03-29",
                                                           "2000-03-29",
                                                           "2000-03-29")),
                                          dob = as.Date(c("2000-02-28",
                                                          "2000-02-29",
                                                          "2000-03-01"))),
                     c(1L, 1L, 0L))
})


## as_ymd ---------------------------------------------------------------------

test_that("'as_ymd' gives correct answer with valid inputs", {
    x <- as.Date(c("2001-03-02", "2000-02-29", NA))
    ans_obtained <- as_ymd(x)
    ans_expected <- list(y = c(2001L, 2000L, NA),
                         m = c(3L, 2L, NA),
                         d = c(2L, 28L, NA))
    expect_identical(ans_obtained, ans_expected)
    x <- as.Date(character())
    ans_obtained <- as_ymd(x)
    ans_expected <- list(y = integer(),
                         m = integer(),
                         d = integer())
    expect_identical(ans_obtained, ans_expected)
})


## date_to_period_or_cohort_year ----------------------------------------------

test_that("'date_to_period_or_cohort_year' gives correct answer with valid inputs", {
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20", "2001-02-11", "2004-12-30"),
                                                   year_to = TRUE,
                                                   break_min = "2001-01-01",
                                                   break_max = NULL,
                                                   open_left = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2004"),
                            levels = as.character(2001:2004)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20", "2001-02-11", "2004-12-30"),
                                                   year_to = TRUE,
                                                   break_min = "2000-07-01",
                                                   break_max = NULL,
                                                   open_left = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2005"),
                            levels = as.character(2001:2005)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20", "2001-02-11", "2004-12-30"),
                                                   year_to = FALSE,
                                                   break_min = "2001-01-01",
                                                   break_max = NULL,
                                                   open_left = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2004"),
                            levels = as.character(2001:2004)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20", "2001-02-11", "2004-12-30"),
                                                   year_to = FALSE,
                                                   break_min = "2000-07-01",
                                                   break_max = NULL,
                                                   open_left = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = as.character(2000:2004)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20", "2001-02-11", "2004-12-30"),
                                                   year_to = FALSE,
                                                   break_min = "2000-07-01",
                                                   break_max = "2005-07-01",
                                                   open_left = TRUE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = c("<2000", as.character(2000:2004))))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20", "2001-02-11", "2004-12-30"),
                                                   year_to = FALSE,
                                                   break_min = NULL,
                                                   break_max = "2005-07-01",
                                                   open_left = TRUE,
                                                   as_factor = FALSE),
                     c("2002", "2000", "2004"))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20", "2001-02-11", NA, "2004-12-30"),
                                                   year_to = FALSE,
                                                   break_min = "2000-07-01",
                                                   break_max = "2005-07-01",
                                                   open_left = TRUE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", NA, "2004"),
                            levels = c("<2000", as.character(2000:2004), NA),
                            exclude = NULL))
})

## date_to_period_or_cohort_multi ----------------------------------------------

test_that("'date_to_period_or_cohort_multi' gives correct answer with valid inputs", {
    expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    break_min = "2000-01-01",
                                                    break_max = NULL,
                                                    open_left = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2005", "2000-2005", "2010-2015"),
                            levels = c("2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2000-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    break_min = NULL,
                                                    break_max = "2015-07-01",
                                                    open_left = FALSE,
                                                    as_factor = TRUE),
                     factor(c("1995-2000", "2000-2005", "2010-2015"),
                            levels = c("1995-2000", "2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2000-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    break_min = "1996-07-01",
                                                    break_max = NULL,
                                                    open_left = FALSE,
                                                    as_factor = TRUE),
                     factor(c("1996-2001", "1996-2001", "2006-2011"),
                            levels = c("1996-2001", "2001-2006", "2006-2011")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2000-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    break_min = NULL,
                                                    break_max = "2011-07-01",
                                                    open_left = FALSE,
                                                    as_factor = FALSE),
                     c("1996-2001", "1996-2001", "2006-2011"))
    expect_identical(date_to_period_or_cohort_multi(date = c("2000-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    break_min = "1996-07-01",
                                                    break_max = NULL,
                                                    open_left = TRUE,
                                                    as_factor = TRUE),
                     factor(c("1996-2001", "1996-2001", "2006-2011"),
                            levels = c("<1996", "1996-2001", "2001-2006", "2006-2011")))
        expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 10,
                                                    break_min = "2000-01-01",
                                                    break_max = NULL,
                                                    open_left = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2010", "2000-2010", "2010-2020"),
                            levels = c("2000-2010", "2010-2020")))
        expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 10,
                                                    break_min = "1990-01-01",
                                                    break_max = NULL,
                                                    open_left = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2010", "2000-2010", "2010-2020"),
                            levels = c("1990-2000", "2000-2010", "2010-2020")))
        expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 10,
                                                    break_min = "1990-01-01",
                                                    break_max = "2020-01-01",
                                                    open_left = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2010", "2000-2010", "2010-2020"),
                            levels = c("1990-2000", "2000-2010", "2010-2020")))
        expect_identical(date_to_period_or_cohort_multi(date = c(NA, "2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 10,
                                                    break_min = "1990-01-01",
                                                    break_max = "2020-01-01",
                                                    open_left = FALSE,
                                                    as_factor = TRUE),
                     factor(c(NA, "2000-2010", "2000-2010", "2010-2020"),
                            levels = c("1990-2000", "2000-2010", "2010-2020", NA),
                            exclude = FALSE))
})

## date_ymd_ge ----------------------------------------------------------------

test_that("'date_ymd_ge' gives correct answer with valid inputs", {
    expect_true(date_ymd_ge(y1 = 2000L, m1 = 7L, d1 = 1L,
                            y2 = 2000L, m2 = 7L, d2 = 1L))
    expect_true(date_ymd_ge(y1 = 2001L, m1 = 7L, d1 = 1L,
                            y2 = 2000L, m2 = 7L, d2 = 1L))
    expect_true(date_ymd_ge(y1 = 2000L, m1 = 8L, d1 = 1L,
                            y2 = 2000L, m2 = 7L, d2 = 1L))
    expect_true(date_ymd_ge(y1 = 2000L, m1 = 7L, d1 = 2L,
                            y2 = 2000L, m2 = 7L, d2 = 1L))
    expect_false(date_ymd_ge(y1 = 2000L, m1 = 12L, d1 = 1L,
                             y2 = 2001L, m2 = 7L, d2 = 1L))
    expect_false(date_ymd_ge(y1 = 2000L, m1 = 7L, d1 = 1L,
                             y2 = 2000L, m2 = 8L, d2 = 1L))
})

## diff_completed__year -------------------------------------------------------

test_that("'diff_completed_year' gives correct answer with valid inputs", {
    expect_identical(diff_completed_year(y1 = 2000L, m1 = 7L, d1 = 1L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 2000L, m1 = 7L, d1 = 2L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 2000L, m1 = 12, d1 = 31L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 2001L, m1 = 06, d1 = 30L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 2001L, m1 = 7L, d1 = 1L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     1L)
    expect_identical(diff_completed_year(y1 = 2001L, m1 = 7L, d1 = 2L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     1L)
    expect_identical(diff_completed_year(y1 = 2000L, m1 = 6L, d1 = 30L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 2000L, m1 = 1L, d1 = 1L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 2000L, m1 = 1L, d1 = 1L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 1999L, m1 = 3L, d1 = 1L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     -1L)
    expect_identical(diff_completed_year(y1 = 1999L, m1 = 8L, d1 = 1L,
                                         y2 = 2000L, m2 = 7L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 1999L, m1 = 8L, d1 = 1L,
                                         y2 = 2000L, m2 = 1L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 1999L, m1 = 1L, d1 = 2L,
                                         y2 = 2000L, m2 = 1L, d2 = 1L),
                     0L)
    expect_identical(diff_completed_year(y1 = 1999L, m1 = 1L, d1 = 1L,
                                         y2 = 2000L, m2 = 1L, d2 = 1L),
                     -1L)
})

## default_break_min ----------------------------------------------------------

test_that("'default_break_min' gives correct answer with valid input", {
    expect_identical(default_break_min(date = as.Date(c("2007-03-22", NA)),
                                       
                                           break_min = "1996-07-01",
                                           break_max = "2011-07-01",
                                           width = 1L),


## make_breaks_date_year ---------------------------------------------------

test_that("'make_breaks_date_year' gives correct answer with valid input", {
    expect_identical(make_breaks_date_year(date = as.Date("2000-01-01"),
                                           break_min = "1996-07-01",
                                           break_max = "2011-07-01",
                                           width = 1L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "1 year"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = "1996-07-01",
                                           break_max = NULL,
                                           width = 5L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "5 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = "1996-07-01",
                                           break_max = NULL,
                                           width = 1L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "1 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = NULL,
                                           break_max = "2015-07-01",
                                           width = 5L),
                     seq.Date(from = as.Date("1995-07-01"),
                              to = as.Date("2015-07-01"),
                              by = "5 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = "1996-01-01",
                                           break_max = NULL,
                                           width = 1L),
                     seq.Date(from = as.Date("1996-01-01"),
                              to = as.Date("2012-01-01"),
                              by = "1 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = "1994-01-01",
                                           break_max = NULL,
                                           width = 3L),
                     seq.Date(from = as.Date("1994-01-01"),
                              to = as.Date("2012-01-01"),
                              by = "3 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = NULL,
                                           break_max = "2020-04-01",
                                           width = 10L),
                     seq.Date(from = as.Date("1990-04-01"),
                              to = as.Date("2020-04-01"),
                              by = "10 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = "1990-04-01",
                                           break_max = NULL,
                                           width = 10L),
                     seq.Date(from = as.Date("1990-04-01"),
                              to = as.Date("2020-04-01"),
                              by = "10 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = "1996-04-01",
                                           break_max = NULL,
                                           width = 2L),
                     seq.Date(from = as.Date("1996-04-01"),
                              to = as.Date("2012-04-01"),
                              by = "2 years"))
})

## make_breaks_integer_lifetab ---------------------------------------------------

test_that("'make_breaks_integer_lifetab' gives correct answer with valid inputs", {
    expect_identical(make_breaks_integer_lifetab(break_max = 100L),
                     c(0L, 1L, seq.int(from = 5L, by = 5L, to = 100L)))
    expect_identical(make_breaks_integer_lifetab(break_max = 5L),
                     c(0L, 1L, 5L))
})


## make_breaks_integer_year ---------------------------------------------------

test_that("'make_breaks_integer_year' gives correct answer when break_max is finite", {
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              width = 5L,
                                              break_max = 100L,
                                              open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              width = 5L,
                                              break_max = 100L,
                                              open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 21L),
                                              width = 5L,
                                              break_max = 100L,
                                              open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 21L, NA),
                                              width = 5L,
                                              break_max = 100L,
                                              open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 100L))
})

test_that("'make_breaks_integer_year' gives correct answer when break_max is Inf", {
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 120L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 120L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 125L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 125L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 80L),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 80L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 84L),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 84L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 84L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 80L))
})

## make_labels_age_group_month_quarter ----------------------------------------

test_that("'make_labels_age_group_month_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_left = FALSE,
                                                         open_right = FALSE,
                                                         unit = "month",
                                                         include_na = FALSE),
                     c("0m", "1m", "2m", "3m", "4m"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_left = FALSE,
                                                         open_right = FALSE,
                                                         unit = "quarter",
                                                         include_na = FALSE),
                     c("0q", "1q", "2q", "3q", "4q"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_left = TRUE,
                                                         open_right = TRUE,
                                                         unit = "month",
                                                         include_na = FALSE),
                     c("<0m", "0m", "1m", "2m", "3m", "4m", "5m+"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_left = FALSE,
                                                         open_right = TRUE,
                                                         unit = "quarter",
                                                         include_na = TRUE),
                     c("0q", "1q", "2q", "3q", "4q", "5q+", NA))
})

test_that("'make_labels_age_group_month_quarter' gives correct error with invalid inputs", {
    expect_error(make_labels_age_group_month_quarter(break_min = 0,
                                               break_max = 5,
                                               open_left = FALSE,
                                               open_right = FALSE,
                                               unit = "wrong",
                                               include_na = FALSE),
                 "can't handle unit 'wrong'")
})

## make_labels_period_month_quarter ----------------------------------------

test_that("'make_labels_period_month_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_period_month_quarter(break_min = "2019-01-01",
                                                      break_max = "2019-04-01",
                                                      open_left = FALSE,
                                                      open_right = FALSE,
                                                      unit = "month",
                                                      include_na = FALSE),
                     c("2019 Jan", "2019 Feb", "2019 Mar"))
    expect_identical(make_labels_period_month_quarter(break_min = "2019-01-01",
                                                      break_max = "2020-01-01",
                                                      open_left = FALSE,
                                                      open_right = FALSE,
                                                      unit = "quarter",
                                                      include_na = FALSE),
                     c("2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4"))
    expect_identical(make_labels_period_month_quarter(break_min = "2019-07-01",
                                                      break_max = "2020-07-01",
                                                      open_left = TRUE,
                                                      open_right = TRUE,
                                                      unit = "quarter",
                                                      include_na = TRUE),
                     c("<2019 Q3", "2019 Q3", "2019 Q4", "2020 Q1", "2020 Q2", "2020 Q3+", NA))
})

test_that("'make_labels_period_month_quarter' gives correct error with invalid inputs", {
    expect_error(make_labels_period_month_quarter(break_min = "2001-03-01",
                                                  break_max = "2001-04-01",
                                                  open_left = FALSE,
                                                  open_right = FALSE,
                                                  unit = "wrong",
                                                  include_na = FALSE),
                 "value for 'unit' \\[\"wrong\"\\] is not a permitted time unit")
})

