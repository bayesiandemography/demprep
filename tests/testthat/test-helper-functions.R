
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


## age_completed_months_start_month -------------------------------------------

test_that("'age_completed_months_start_month' gives correct answer with valid inputs", {
    date <- demprep:::as_ymd(as.Date("2001-01-01"))
    dob <- demprep:::as_ymd(as.Date("2000-01-01"))
    expect_identical(age_completed_months_start_month(date = date,
                                                      dob = dob),
                     12L)
    date <- demprep:::as_ymd(as.Date("2001-01-01"))
    dob <- demprep:::as_ymd(as.Date("2000-01-15"))
    expect_identical(age_completed_months_start_month(date = date,
                                                      dob = dob),
                     11L)
    date <- demprep:::as_ymd(as.Date("2001-01-31"))
    dob <- demprep:::as_ymd(as.Date("2000-01-15"))
    expect_identical(age_completed_months_start_month(date = date,
                                                      dob = dob),
                     11L)
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
    expect_identical(as_ymd(c("2000-01-01", NA)),
                     list(y = c(2000L, NA),
                          m = c(1L, NA),
                          d = c(1L, NA)))
})



## date_to_period_or_cohort_month -------------------------------------------

test_that("'date_to_period_or_cohort_month' gives correct answer with valid inputs", {
    expect_identical(date_to_period_or_cohort_month(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    break_min = as.Date("2000-01-01"),
                                                    open_first = TRUE,
                                                    as_factor = TRUE),
                     factor(c("2003 Mar", "2001 Feb", "2010 Dec"),
                            levels = c("<2000 Jan", paste(rep(2000:2010, each = 12), month.abb))))
    expect_identical(date_to_period_or_cohort_month(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                      break_min = NULL,
                                                      open_first = FALSE,
                                                      as_factor = TRUE),
                     factor(c("2003 Mar", "2001 Feb", "2010 Dec"),
                            levels = paste(rep(2001:2010, each = 12), month.abb)[-1]))
    expect_identical(date_to_period_or_cohort_month(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                      break_min = NULL,
                                                      open_first = FALSE,
                                                      as_factor = FALSE),
                     c("2003 Mar", "2001 Feb", "2010 Dec"))
    expect_identical(date_to_period_or_cohort_month(date = c("2000-01-01", "2000-01-01"),
                                                      break_min = NULL,
                                                      open_first = FALSE,
                                                      as_factor = TRUE),
                     factor(c("2000 Jan", "2000 Jan"),
                            levels = "2000 Jan"))
    expect_identical(date_to_period_or_cohort_month(date = c("2000-01-01", NA, "2000-01-01"),
                                                      break_min = NULL,
                                                      open_first = FALSE,
                                                      as_factor = TRUE),
                     factor(c("2000 Jan", NA, "2000 Jan"),
                            levels = c("2000 Jan")))
})


## date_to_period_or_cohort_multi ----------------------------------------------

test_that("'date_to_period_or_cohort_multi' gives correct answer with valid inputs", {
    expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    origin = 2000,
                                                    month_start = "Jan",
                                                    break_min = as.Date("2000-01-01"),
                                                    open_first = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2005", "2000-2005", "2010-2015"),
                            levels = c("2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    origin = 2000,
                                                    month_start = "Jan",
                                                    break_min = NULL,
                                                    open_first = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2005", "2000-2005", "2010-2015"),
                            levels = c("2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2000-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    origin = 0,
                                                    month_start = "Jul",
                                                    break_min = NULL,
                                                    open_first = FALSE,
                                                    as_factor = TRUE),
                     factor(c("1995-2000", "2000-2005", "2010-2015"),
                            levels = c("1995-2000", "2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2000-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    month_start = "Jul",
                                                    break_min = as.Date("1996-07-01"),
                                                    origin = 2001,
                                                    open_first = FALSE,
                                                    as_factor = TRUE),
                     factor(c("1996-2001", "1996-2001", "2006-2011"),
                            levels = c("1996-2001", "2001-2006", "2006-2011")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2000-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    origin = 1996,
                                                    month_start = "Jul",
                                                    break_min = NULL,
                                                    open_first = FALSE,
                                                    as_factor = FALSE),
                     c("1996-2001", "1996-2001", "2006-2011"))
    expect_identical(date_to_period_or_cohort_multi(date = c("2000-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 5,
                                                    origin = 2001,
                                                    month_start = "Apr",
                                                    break_min = NULL,
                                                    open_first = TRUE,
                                                    as_factor = TRUE),
                     factor(c("1996-2001", "1996-2001", "2006-2011"),
                            levels = c("<1996", "1996-2001", "2001-2006", "2006-2011")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 10,
                                                    break_min = as.Date("2000-01-01"),
                                                    origin = 2000,
                                                    month_start = "Jan",
                                                    open_first = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2010", "2000-2010", "2010-2020"),
                            levels = c("2000-2010", "2010-2020")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 10,
                                                    origin = 1990,
                                                    month_start = "Jan",
                                                    break_min = NULL,
                                                    open_first = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2010", "2000-2010", "2010-2020"),
                            levels = c("2000-2010", "2010-2020")))
    expect_identical(date_to_period_or_cohort_multi(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 10,
                                                    break_min = as.Date("1990-01-01"),
                                                    origin = 1990,
                                                    month_start = "Jan",
                                                    open_first = FALSE,
                                                    as_factor = TRUE),
                     factor(c("2000-2010", "2000-2010", "2010-2020"),
                            levels = c("1990-2000", "2000-2010", "2010-2020")))
    expect_identical(date_to_period_or_cohort_multi(date = c(NA, "2003-03-20", "2001-02-11", "2010-12-30"),
                                                    width = 10,
                                                    month_start = "Jan",
                                                    origin = 2000,
                                                    break_min = as.Date("1990-01-01"),
                                                    open_first = FALSE,
                                                    as_factor = TRUE),
                     factor(c(NA, "2000-2010", "2000-2010", "2010-2020"),
                            levels = c("1990-2000", "2000-2010", "2010-2020")))
})


## date_to_period_or_cohort_year ----------------------------------------------

test_that("'date_to_period_or_cohort_year' gives correct answer with valid inputs", {
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   month_start = "Jan",
                                                   break_min = as.Date("2001-01-01"),
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2004"),
                            levels = as.character(2001:2004)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   month_start = "Jan",
                                                   break_min = NULL,
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2004"),
                            levels = as.character(2001:2004)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = as.character(2000:2004)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = as.character(2000:2004)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = FALSE,
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2005"),
                            levels = as.character(2001:2005)))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   break_min = as.Date("2000-07-01"),
                                                   month_start = "Jul",
                                                   open_first = TRUE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = c("<2000", as.character(2000:2004))))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   month_start = "Jul",
                                                   break_min = as.Date("2005-07-01"),
                                                   open_first = TRUE,
                                                   as_factor = FALSE),
                     c("<2005", "<2005", "<2005"))
    expect_identical(date_to_period_or_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            NA,
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   break_min = as.Date("2000-07-01"),
                                                   month_start = "Jul",
                                                   open_first = TRUE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", NA, "2004"),
                            levels = c("<2000", as.character(2000:2004))))
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

## diff_completed_year -------------------------------------------------------

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


## i_month_within_period ------------------------------------------------------

test_that("'i_month_within_period' gives correct answer with valid inputs", {
    expect_identical(i_month_within_period(date_ymd = as_ymd("2000-01-01"),
                                           width = 1L,
                                           origin = 2000L,
                                           month_start = "Jan"),
                     1L)
    expect_identical(i_month_within_period(date_ymd = as_ymd("1999-12-31"),
                                           width = 1L,
                                           origin = 2000L,
                                           month_start = "Jan"),
                     12L)
    expect_identical(i_month_within_period(date_ymd = as_ymd("1999-12-31"),
                                           width = 5L,
                                           origin = 2000L,
                                           month_start = "Jan"),
                     60L)
    expect_identical(i_month_within_period(date_ymd = as_ymd("2000-01-01"),
                                           width = 1L,
                                           origin = 1998L,
                                           month_start = "Jan"),
                     1L)
    expect_identical(i_month_within_period(date_ymd = as_ymd("2000-01-01"),
                                           width = 2L,
                                           origin = 1998L,
                                           month_start = "Jul"),
                     19L)
    expect_identical(i_month_within_period(date_ymd = as_ymd(NA),
                                           width = 2L,
                                           origin = 1998L,
                                           month_start = "Jul"),
                     NA_integer_)
    expect_identical(i_month_within_period(date_ymd = as_ymd("2000-06-30"),
                                           width = 5L,
                                           origin = 2000L,
                                           month_start = "Jul"),
                     60L)
    expect_identical(i_month_within_period(date_ymd = as_ymd("2000-07-01"),
                                           width = 5L,
                                           origin = 2000L,
                                           month_start = "Jul"),
                     1L)
})


## is_lower_within_month ------------------------------------------------------

test_that("'is_lower_within_month' gives correct answer with valid input", {
    date_ymd <- demprep:::as_ymd(seq.Date(from = as.Date("2001-01-01"), by = "day", length.out = 10))
    dob_ymd <- demprep:::as_ymd(seq.Date(from = as.Date("2000-02-01"), by = "day", length.out = 10))
    expect_identical(is_lower_within_month(date_ymd = date_ymd,
                                           dob_ymd = dob_ymd),
                     rep(c(TRUE, FALSE), times = 5))
    date_ymd <- demprep:::as_ymd(as.Date("2000-01-02"))
    dob_ymd <- demprep:::as_ymd(as.Date("2019-12-01"))
    expect_true(is_lower_within_month(date_ymd = date_ymd,
                                      dob_ymd = dob_ymd))
    date_ymd <- demprep:::as_ymd(as.Date("2019-12-01"))
    dob_ymd <- demprep:::as_ymd(as.Date("2000-01-02"))
    expect_false(is_lower_within_month(date_ymd = date_ymd,
                                       dob_ymd = dob_ymd))
})
                                           
                                           
## make_breaks_date_month -----------------------------------------------------

test_that("'make_breaks_date_month' gives correct answer with valid input", {
    expect_identical(make_breaks_date_month(date = as.Date("2001-01-05"),
                                              break_min = NULL),
                     as.Date(c("2001-01-01", "2001-02-01")))
    expect_identical(make_breaks_date_month(date = as.Date(c("2001-01-05", "2000-02-29")),
                                              break_min = NULL),
                     seq.Date(from = as.Date("2000-02-01"),
                              to = as.Date("2001-02-01"),
                              by = "month"))
    expect_identical(make_breaks_date_month(date = as.Date(c("2001-01-05", NA, "2000-02-29")),
                                              break_min = NULL),
                     seq.Date(from = as.Date("2000-02-01"),
                              to = as.Date("2001-02-01"),
                              by = "month"))
    expect_identical(make_breaks_date_month(date = as.Date(c("2001-01-05", NA, "2005-12-29")),
                                              break_min = NULL),
                     seq.Date(from = as.Date("2001-01-01"),
                              to = as.Date("2006-01-01"),
                              by = "month"))
    expect_identical(make_breaks_date_month(date = as.Date("2001-01-05"),
                                              break_min = as.Date("2000-01-01")),
                     seq.Date(from = as.Date("2000-01-01"),
                              to = as.Date("2001-02-01"),
                              by = "month"))
    expect_identical(make_breaks_date_month(date = as.Date(c("2001-01-05", "2000-02-29")),
                                              break_min = as.Date("1999-12-01")),
                     seq.Date(from = as.Date("1999-12-01"),
                              to = as.Date("2001-02-01"),
                              by = "month"))
})


## make_breaks_date_quarter ---------------------------------------------------

test_that("'make_breaks_date_quarter' gives correct answer with valid input", {
    expect_identical(make_breaks_date_quarter(date = as.Date("2001-01-05"),
                                              break_min = NULL),
                     as.Date(c("2001-01-01", "2001-04-01")))
    expect_identical(make_breaks_date_quarter(date = as.Date(c("2001-01-05", "2000-02-29")),
                                              break_min = NULL),
                     seq.Date(from = as.Date("2000-01-01"),
                              to = as.Date("2001-04-01"),
                              by = "quarter"))
    expect_identical(make_breaks_date_quarter(date = as.Date(c("2001-01-05", NA, "2000-02-29")),
                                              break_min = NULL),
                     seq.Date(from = as.Date("2000-01-01"),
                              to = as.Date("2001-04-01"),
                              by = "quarter"))
    expect_identical(make_breaks_date_quarter(date = as.Date(c("2001-01-05", NA, "2005-12-29")),
                                              break_min = NULL),
                     seq.Date(from = as.Date("2001-01-01"),
                              to = as.Date("2006-01-01"),
                              by = "quarter"))
    expect_identical(make_breaks_date_quarter(date = as.Date("2001-01-05"),
                                              break_min = as.Date("2000-01-01")),
                     seq.Date(from = as.Date("2000-01-01"),
                              to = as.Date("2001-04-01"),
                              by = "quarter"))
    expect_identical(make_breaks_date_quarter(date = as.Date(c("2001-01-05", "2000-02-29")),
                                              break_min = as.Date("1999-10-01")),
                     seq.Date(from = as.Date("1999-10-01"),
                              to = as.Date("2001-04-01"),
                              by = "quarter"))
})


## make_breaks_date_year ---------------------------------------------------

test_that("'make_breaks_date_year' gives correct answer with valid input", {
    expect_identical(make_breaks_date_year(date = as.Date("2001-01-05"),
                                           break_min = as.Date("1996-01-01"),
                                           month_start = "Jan",
                                           origin = NULL,
                                           width = 1L),
                     seq.Date(from = as.Date("1996-01-01"),
                              to = as.Date("2002-01-01"),
                              by = "1 year"))
    expect_identical(make_breaks_date_year(date = as.Date("2001-01-05"),
                                           break_min = NULL,
                                           month_start = "Jan",
                                           origin = NULL,
                                           width = 1L),
                     seq.Date(from = as.Date("2001-01-01"),
                              to = as.Date("2002-01-01"),
                              by = "1 year"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-08-03", "2010-12-31")),
                                           break_min = NULL,
                                           month_start = "Jul",
                                           origin = NULL,
                                           width = 1L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "1 year"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = NULL,
                                           month_start = "Jul",
                                           origin = 2001,
                                           width = 5L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "5 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = as.Date("1996-07-01"),
                                           origin = 2000L,
                                           month_start = "Jul",
                                           width = 1L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "1 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           origin = 2000,
                                           break_min = NULL,
                                           month_start = "Jul",
                                           width = 5L),
                     seq.Date(from = as.Date("1995-07-01"),
                              to = as.Date("2015-07-01"),
                              by = "5 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = NULL,
                                           origin = 2011,
                                           month_start = "Jan",
                                           width = 1L),
                     seq.Date(from = as.Date("1996-01-01"),
                              to = as.Date("2012-01-01"),
                              by = "1 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = NULL,
                                           month_start = "Jan",
                                           origin = 2000,
                                           width = 3L),
                     seq.Date(from = as.Date("1994-01-01"),
                              to = as.Date("2012-01-01"),
                              by = "3 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = NULL,
                                           month_start = "Apr",
                                           origin = 2020,
                                           width = 10L),
                     seq.Date(from = as.Date("1990-04-01"),
                              to = as.Date("2020-04-01"),
                              by = "10 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           origin = 1990,
                                           month_start = "Apr",
                                           break_min = NULL,
                                           width = 10L),
                     seq.Date(from = as.Date("1990-04-01"),
                              to = as.Date("2020-04-01"),
                              by = "10 years"))
    expect_identical(make_breaks_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                           break_min = as.Date("1996-04-01"),
                                           origin = 2000L,
                                           month_start = "Apr",
                                           width = 2L),
                     seq.Date(from = as.Date("1996-04-01"),
                              to = as.Date("2012-04-01"),
                              by = "2 years"))
    expect_identical(make_breaks_date_year(date = as.Date(character()),
                                           origin = 2000L,
                                           month_start = "Apr",
                                           width = 2L,
                                           break_min = NULL),
                     as.Date(c("2000-04-01", "2002-04-01")))
    expect_identical(make_breaks_date_year(date = as.Date(character()),
                                           origin = 2000L,
                                           month_start = "Apr",
                                           width = 2L,
                                           break_min = as.Date("1996-04-01")),
                     as.Date(c("1996-04-01", "1998-04-01")))
})


## make_breaks_integer_fert ---------------------------------------------------

test_that("'make_breaks_integer_fert' gives correct answer when break_max is finite", {
    expect_identical(make_breaks_integer_fert(age = c(17L, 22L, 37L),
                                              width = 5L,
                                              break_min = 15L,
                                              break_max = 50L),
                     seq.int(from = 15L, by = 5L, to = 50L))
    expect_identical(make_breaks_integer_fert(age = c(17L, 22L, 37L, NA),
                                              width = 5L,
                                              break_min = 15L,
                                              break_max = 50L),
                     seq.int(from = 15L, by = 5L, to = 50L))
    expect_identical(make_breaks_integer_fert(age = c(17L, 22L, 37L, NA),
                                              width = 5L,
                                              break_min = NULL,
                                              break_max = NULL),
                     seq.int(from = 15L, by = 5L, to = 40L))
    expect_identical(make_breaks_integer_fert(age = c(17L, 22L, 37L, NA),
                                              width = 5L,
                                              break_min = NULL,
                                              break_max = 45L),
                     seq.int(from = 15L, by = 5L, to = 45L))
})


## make_breaks_integer_lifetab ---------------------------------------------------

test_that("'make_breaks_integer_lifetab' gives correct answer with valid inputs", {
    expect_identical(make_breaks_integer_lifetab(break_max = 100L),
                     c(0L, 1L, seq.int(from = 5L, by = 5L, to = 100L)))
    expect_identical(make_breaks_integer_lifetab(break_max = 5L),
                     c(0L, 1L, 5L))
})


## make_breaks_integer_month_quarter ---------------------------------------------------

test_that("'make_breaks_integer_month_quarter' gives correct answer when break_max is non-NULL", {
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 121L),
                                                       break_max = 100L,
                                                       open_last = TRUE),
                     seq.int(from = 0L, to = 100L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 121L, NA),
                                                       break_max = 100L,
                                                       open_last = TRUE),
                     seq.int(from = 0L, to = 100L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 21L),
                                                       break_max = 100L,
                                                       open_last = FALSE),
                     seq.int(from = 0L, to = 100L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 21L, NA),
                                                       break_max = 100L,
                                                       open_last = FALSE),
                     seq.int(from = 0L, to = 100L))
})

test_that("'make_breaks_integer_month_quarter' gives correct answer when break_max is NULL", {
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 121L),
                                              break_max = NULL,
                                              open_last = TRUE),
                     seq.int(from = 0L, to = 121L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 121L, NA),
                                              break_max = NULL,
                                              open_last = TRUE),
                     seq.int(from = 0L, to = 121L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 121L),
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, to = 122L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 121L, NA),
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, to = 122L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 80L),
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, to = 81L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 80L, NA),
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, to = 81L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 84L),
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, to = 85L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 84L, NA),
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, to = 85L))
    expect_identical(make_breaks_integer_month_quarter(age = c(50L, 22L, 84L, NA),
                                              break_max = NULL,
                                              open_last = TRUE),
                     seq.int(from = 0L, to = 84L))
})


## make_breaks_integer_year ---------------------------------------------------

test_that("'make_breaks_integer_year' gives correct answer when break_max is non-NULL", {
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              width = 5L,
                                              break_max = 100L,
                                              open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              width = 5L,
                                              break_max = 100L,
                                              open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 21L),
                                              width = 5L,
                                              break_max = 100L,
                                              open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 21L, NA),
                                              width = 5L,
                                              break_max = 100L,
                                              open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 100L))
})

test_that("'make_breaks_integer_year' gives correct answer when break_max is NULL", {
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 120L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 120L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 125L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 125L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 80L),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 80L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 84L),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 84L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 84L, NA),
                                              width = 5L,
                                              break_max = NULL,
                                              open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 80L))
})

## make_fill ------------------------------------------------------------------

test_that("'make_fill gives correct answer with valid inputs", {
    X <- 1:5
    INDEX <- data.frame(a = factor(5:1))
    expect_identical(make_fill(fill = 0L,
                               X = X,
                               INDEX = INDEX),
                     0L)
    expect_identical(make_fill(fill = 0,
                               X = X,
                               INDEX = INDEX),
                     0L)
    expect_identical(make_fill(fill = 999,
                               X = X,
                               INDEX = INDEX),
                     999L)
    expect_identical(make_fill(fill = NA,
                               X = X,
                               INDEX = INDEX),
                     NA_integer_)
    expect_identical(make_fill(fill = NULL,
                               X = X,
                               INDEX = INDEX),
                     0L)
    expect_identical(make_fill(fill = NULL,
                               X = -X,
                               INDEX = INDEX),
                     NA_integer_)
    expect_identical(make_fill(fill = NULL,
                               X = rep(0.1, 5),
                               INDEX = INDEX),
                     0L)
})

test_that("'make_fill gives correct error with invalid inputs", {
    X <- c(0.1, 0.3, 0.2)
    INDEX <- data.frame(a = factor(c(1, 2, 1)), b = factor(c(10, 10, 11)))
    expect_error(make_fill(fill = NULL,
                           X = X,
                           INDEX = INDEX),
                 paste("some combinations of the cross-classifying variables are not included",
                       "in the data, but no value for 'fill' has been supplied"))
    expect_error(make_fill(fill = "a",
                           X = X,
                           INDEX = INDEX),
                 "invalid value for 'fill'")
})

## make_labels_age_group_month_quarter ----------------------------------------

test_that("'make_labels_age_group_month_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_last = FALSE,
                                                         unit = "month",
                                                         include_na = FALSE),
                     c("0m", "1m", "2m", "3m", "4m"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_last = FALSE,
                                                         unit = "quarter",
                                                         include_na = FALSE),
                     c("0q", "1q", "2q", "3q", "4q"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_last = TRUE,
                                                         unit = "month",
                                                         include_na = FALSE),
                     c("0m", "1m", "2m", "3m", "4m", "5m+"))
    expect_identical(make_labels_age_group_month_quarter(break_min = 0,
                                                         break_max = 5,
                                                         open_last = TRUE,
                                                         unit = "quarter",
                                                         include_na = TRUE),
                     c("0q", "1q", "2q", "3q", "4q", "5q+", NA))
})

test_that("'make_labels_age_group_month_quarter' gives correct error with invalid inputs", {
    expect_error(make_labels_age_group_month_quarter(break_min = 0,
                                                     break_max = 5,
                                                     open_last = FALSE,
                                                     unit = "wrong",
                                                     include_na = FALSE),
                 "can't handle unit 'wrong'")
})

## make_labels_period_month_quarter ----------------------------------------

test_that("'make_labels_period_month_quarter' gives correct answer with valid inputs", {
    expect_identical(make_labels_period_month_quarter(break_min = "2019-01-01",
                                                      break_max = "2019-04-01",
                                                      open_first = FALSE,
                                                      unit = "month",
                                                      include_na = FALSE),
                     c("2019 Jan", "2019 Feb", "2019 Mar"))
    expect_identical(make_labels_period_month_quarter(break_min = "2019-01-01",
                                                      break_max = "2020-01-01",
                                                      open_first = FALSE,
                                                      unit = "quarter",
                                                      include_na = FALSE),
                     c("2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4"))
    expect_identical(make_labels_period_month_quarter(break_min = "2019-07-01",
                                                      break_max = "2020-07-01",
                                                      open_first = TRUE,
                                                      unit = "quarter",
                                                      include_na = TRUE),
                     c("<2019 Q3", "2019 Q3", "2019 Q4", "2020 Q1", "2020 Q2", NA))
})

test_that("'make_labels_period_month_quarter' gives correct error with invalid inputs", {
    expect_error(make_labels_period_month_quarter(break_min = "2001-03-01",
                                                  break_max = "2001-04-01",
                                                  open_first = FALSE,
                                                  unit = "wrong",
                                                  include_na = FALSE),
                 "value for 'unit' \\[\"wrong\"\\] is not a permitted time unit")
})

