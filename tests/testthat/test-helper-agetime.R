
context("helper-agetime")

## add_months ----------------------------------------------------------------

test_that("'add_months' gives correct answer with valid inputs", {
    add_months <- demprep:::add_months
    expect_identical(add_months(date = as.Date("2000-01-31"), n = 0L),
                     as.Date("2000-01-31"))
    expect_identical(add_months(date = as.Date("2000-01-31"), n = 1L),
                     as.Date("2000-03-01"))
    expect_identical(add_months(date = as.Date("2000-01-31"), n = 13L),
                     as.Date("2001-03-01"))
    expect_identical(add_months(date = as.Date("2000-03-31"), n = -1L),
                     as.Date("2000-02-29"))
    expect_identical(add_months(date = as.Date("2000-03-31"), n = -49L),
                     as.Date("1996-02-29"))    
    expect_identical(add_months(date = as.Date("2000-03-31"), n = -50L),
                     as.Date("1996-01-31"))
    expect_identical(add_months(date = as.Date("2000-10-31"), n = 1L),
                     as.Date("2000-12-01"))
    expect_identical(add_months(date = as.Date("2000-11-30"), n = 1L),
                     as.Date("2000-12-30"))
    expect_identical(add_months(date = as.Date("2000-12-30"), n = 1L),
                     as.Date("2001-01-30"))
    expect_identical(add_months(date = as.Date("2002-04-30"), n = 19L),
                     as.Date("2003-11-30"))
    expect_identical(add_months(date = as.Date("2002-03-31"), n = 20L),
                     as.Date("2003-12-01"))
    expect_identical(add_months(date = as.Date(character()), n = 20L),
                     as.Date(character()))
    expect_identical(add_months(date = as.Date(c("1900-03-01", "2000-03-01")),
                                n = c(2L, 2L)),
                     as.Date(c("1900-05-01", "2000-05-01")))
    expect_identical(add_months(date = as.Date(c(NA, "2000-03-01")),
                                n = c(2L, 2L)),
                     as.Date(c(NA, "2000-05-01")))
    expect_identical(add_months(date = as.Date(c("1900-03-01", "2000-03-01")),
                                n = c(2L, NA)),
                     as.Date(c("1900-05-01", NA)))
})


## add_quarters ---------------------------------------------------------------

test_that("'add_quarters' gives correct answer with valid inputs", {
    add_quarters <- demprep:::add_quarters
    expect_identical(add_quarters(date = as.Date("2000-01-31"), n = 0L),
                     as.Date("2000-01-31"))
    expect_identical(add_quarters(date = as.Date("2000-01-31"), n = 1L),
                     as.Date("2000-05-01"))
    expect_identical(add_quarters(date = as.Date("2000-02-29"), n = 1L),
                     as.Date("2000-05-29"))
    expect_identical(add_quarters(date = as.Date("2003-11-30"), n = 1L),
                     as.Date("2004-03-01"))
    expect_identical(add_quarters(date = as.Date("2003-11-30"), n = 5L),
                     as.Date("2005-03-01"))
    expect_identical(add_quarters(date = as.Date("2003-11-30"), n = -5L),
                     as.Date("2002-08-30"))
    expect_identical(add_quarters(date = as.Date(character()), n = -5L),
                     as.Date(character()))
    expect_identical(add_quarters(date = as.Date(NA), n = -5L),
                     as.Date(NA))
    expect_identical(add_quarters(date = as.Date("2003-11-30"), n = NA_integer_),
                     as.Date(NA))
})


## add_years ------------------------------------------------------------------

test_that("'add_years' gives correct answer with valid inputs", {
    add_years <- demprep:::add_years
    expect_identical(add_years(date = as.Date("2000-01-31"), n = 0L),
                     as.Date("2000-01-31"))
    expect_identical(add_years(date = as.Date(c("2000-01-31", "2000-02-29")), n = 1L),
                     as.Date(c("2001-01-31", "2001-03-01")))
    expect_identical(add_years(date = as.Date(c("2000-01-31", "2000-02-29")), n = -1L),
                     as.Date(c("1999-01-31", "1999-02-28")))
    expect_identical(add_years(date = as.Date(character()), n = -1L),
                     as.Date(character()))
    expect_identical(add_years(date = as.Date("2000-01-31"), n = 10L),
                     as.Date("2010-01-31"))
    expect_identical(add_years(date = as.Date("2000-02-29"), n = 12L),
                     as.Date("2012-02-29"))
    expect_identical(add_years(date = as.Date("2000-02-28"), n = 12L),
                     as.Date("2012-02-28"))
    expect_identical(add_years(date = as.Date(NA), n = 12L),
                     as.Date(NA))
    expect_identical(add_years(date = as.Date("2000-02-28"), n = NA_integer_),
                     as.Date(NA))
})


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
                     0L)
    expect_identical(age_completed_months(date = as.Date("2000-03-29"),
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
    expect_identical(age_completed_months(date = as.Date(c("2000-02-28",
                                                           "2000-02-29",
                                                           "2000-03-01")),
                                          dob = as.Date(c("2000-01-31",
                                                          "2000-01-31",
                                                          "2000-01-31"))),
                     c(0L, 0L, 1L))
})


## age_completed_months_start_month -------------------------------------------

test_that("'age_completed_months_start_month' gives correct answer with valid inputs", {
    as_ymd <- demprep:::as_ymd
    date <- as_ymd(as.Date("2001-01-01"))
    dob <- as_ymd(as.Date("2000-01-01"))
    expect_identical(age_completed_months_start_month(date = date,
                                                      dob = dob),
                     12L)
    date <- as_ymd(as.Date("2001-01-01"))
    dob <- as_ymd(as.Date("2000-01-15"))
    expect_identical(age_completed_months_start_month(date = date,
                                                      dob = dob),
                     11L)
    date <- as_ymd(as.Date("2001-01-31"))
    dob <- as_ymd(as.Date("2000-01-15"))
    expect_identical(age_completed_months_start_month(date = date,
                                                      dob = dob),
                     11L)
})


## age_completed_years ------------------------------------------------------

test_that("'age_completed_years' gives correct answer with valid inputs", {
    age_completed_years <- demprep:::age_completed_years
    expect_identical(age_completed_years(date = as.Date("2000-01-01"),
                                         dob = as.Date("2000-01-01")),
                     0L)
    expect_identical(age_completed_years(date = as.Date(c("2001-02-28",
                                                          "2001-03-01",
                                                          "2004-02-28",
                                                          "2004-02-29")),
                                         dob = as.Date(c("2000-02-29",
                                                         "2000-02-29",
                                                         "2000-02-29",
                                                         "2000-02-29"))),
                     c(0L, 1L, 3L, 4L))
    expect_identical(age_completed_years(date = as.Date(c("2002-02-28",
                                                          "2001-03-01",
                                                          "2004-02-28",
                                                          "2004-02-29")),
                                         dob = as.Date(c("2001-02-28",
                                                         "2001-02-28",
                                                         "2001-02-28",
                                                         "2001-02-28"))),
                     c(1L, 0L, 3L, 3L))
    expect_identical(age_completed_years(date = as.Date(character()),
                                         dob = as.Date(character())),
                     integer())
})


## as_ymd ---------------------------------------------------------------------

test_that("'as_ymd' gives correct answer with valid inputs", {
    as_ymd <- demprep:::as_ymd
    x <- as.Date(c("2001-03-02", "2000-02-29", NA))
    ans_obtained <- as_ymd(x)
    ans_expected <- list(y = c(2001L, 2000L, NA),
                         m = c(3L, 2L, NA),
                         d = c(2L, 29L, NA))
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


## is_leap_year ---------------------------------------------------------------

test_that("'is_leap_year' gives correct answer with valid input", {
    is_leap_year <- demprep:::is_leap_year
    expect_identical(is_leap_year(c(1900L, 2000L, 2001, 2004L)),
                     c(FALSE, TRUE, FALSE, TRUE))
    expect_identical(is_leap_year(integer()),
                     logical())                                  
})    


## is_lower_within_month ------------------------------------------------------

test_that("'is_lower_within_month' gives correct answer with valid input", {
    as_ymd <- demprep:::as_ymd
    date_ymd <- as_ymd(seq.Date(from = as.Date("2001-01-01"),
                                by = "day",
                                length.out = 10))
    dob_ymd <- as_ymd(seq.Date(from = as.Date("2000-02-01"),
                               by = "day",
                               length.out = 10))
    expect_identical(is_lower_within_month(date_ymd = date_ymd,
                                           dob_ymd = dob_ymd),
                     rep(c(TRUE, FALSE), times = 5))
    date_ymd <- as_ymd(as.Date("2000-01-02"))
    dob_ymd <- as_ymd(as.Date("2019-12-01"))
    expect_true(is_lower_within_month(date_ymd = date_ymd,
                                      dob_ymd = dob_ymd))
    date_ymd <- as_ymd(as.Date("2019-12-01"))
    dob_ymd <- as_ymd(as.Date("2000-01-02"))
    expect_false(is_lower_within_month(date_ymd = date_ymd,
                                       dob_ymd = dob_ymd))
})
                                           
                                           
## make_breaks_date_to_date_month ---------------------------------------------

test_that("'make_breaks_date_to_date_month' gives correct answer with valid input", {
    expect_identical(make_breaks_date_to_date_month(date = as.Date("2001-01-05"),
                                                    break_min = NULL),
                     as.Date(c("2001-01-01", "2001-02-01")))
    expect_identical(make_breaks_date_to_date_month(date = as.Date(c("2001-01-05", "2000-02-29")),
                                                    break_min = NULL),
                     seq.Date(from = as.Date("2000-02-01"),
                              to = as.Date("2001-02-01"),
                              by = "month"))
    expect_identical(make_breaks_date_to_date_month(date = as.Date(c("2001-01-05", NA, "2000-02-29")),
                                                    break_min = NULL),
                     seq.Date(from = as.Date("2000-02-01"),
                              to = as.Date("2001-02-01"),
                              by = "month"))
    expect_identical(make_breaks_date_to_date_month(date = as.Date(c("2001-01-05", NA, "2005-12-29")),
                                                    break_min = NULL),
                     seq.Date(from = as.Date("2001-01-01"),
                              to = as.Date("2006-01-01"),
                              by = "month"))
    expect_identical(make_breaks_date_to_date_month(date = as.Date("2001-01-05"),
                                                    break_min = as.Date("2000-01-01")),
                     seq.Date(from = as.Date("2000-01-01"),
                              to = as.Date("2001-02-01"),
                              by = "month"))
    expect_identical(make_breaks_date_to_date_month(date = as.Date(c("2001-01-05", "2000-02-29")),
                                                    break_min = as.Date("1999-12-01")),
                     seq.Date(from = as.Date("1999-12-01"),
                              to = as.Date("2001-02-01"),
                              by = "month"))
    expect_identical(make_breaks_date_to_date_month(date = as.Date(c(NA, NA)),
                                                    break_min = as.Date("1999-12-01")),
                     as.Date("1999-12-01"))
    expect_identical(make_breaks_date_to_date_month(date = as.Date(character()),
                                                    break_min = as.Date("1999-12-01")),
                     as.Date("1999-12-01"))
})


## make_breaks_date_to_date_quarter -------------------------------------------

test_that("'make_breaks_date_to_date_quarter' gives correct answer with valid input", {
    expect_identical(make_breaks_date_to_date_quarter(date = as.Date("2001-01-05"),
                                                      break_min = NULL),
                     as.Date(c("2001-01-01", "2001-04-01")))
    expect_identical(make_breaks_date_to_date_quarter(date = as.Date(c("2001-01-05", "2000-02-29")),
                                                      break_min = NULL),
                     seq.Date(from = as.Date("2000-01-01"),
                              to = as.Date("2001-04-01"),
                              by = "quarter"))
    expect_identical(make_breaks_date_to_date_quarter(date = as.Date(c("2001-01-05", NA, "2000-02-29")),
                                                      break_min = NULL),
                     seq.Date(from = as.Date("2000-01-01"),
                              to = as.Date("2001-04-01"),
                              by = "quarter"))
    expect_identical(make_breaks_date_to_date_quarter(date = as.Date(c("2001-01-05", NA, "2005-12-29")),
                                                      break_min = NULL),
                     seq.Date(from = as.Date("2001-01-01"),
                              to = as.Date("2006-01-01"),
                              by = "quarter"))
    expect_identical(make_breaks_date_to_date_quarter(date = as.Date("2001-01-05"),
                                                      break_min = as.Date("2000-01-01")),
                     seq.Date(from = as.Date("2000-01-01"),
                              to = as.Date("2001-04-01"),
                              by = "quarter"))
    expect_identical(make_breaks_date_to_date_quarter(date = as.Date(c("2001-01-05", "2000-02-29")),
                                                      break_min = as.Date("1999-10-01")),
                     seq.Date(from = as.Date("1999-10-01"),
                              to = as.Date("2001-04-01"),
                              by = "quarter"))
})


## make_breaks_date_to_date_year ----------------------------------------------

test_that("'make_breaks_date_to_date_year' gives correct answer with valid input", {
    expect_identical(make_breaks_date_to_date_year(date = as.Date("2001-01-05"),
                                                   break_min = as.Date("1996-01-01"),
                                                   month_start = "Jan",
                                                   origin = NULL,
                                                   width = 1L),
                     seq.Date(from = as.Date("1996-01-01"),
                              to = as.Date("2002-01-01"),
                              by = "1 year"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date("2001-01-05"),
                                                   break_min = NULL,
                                                   month_start = "Jan",
                                                   origin = NULL,
                                                   width = 1L),
                     seq.Date(from = as.Date("2001-01-01"),
                              to = as.Date("2002-01-01"),
                              by = "1 year"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-08-03", "2010-12-31")),
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   origin = NULL,
                                                   width = 1L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "1 year"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   origin = 2001,
                                                   width = 5L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "5 years"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                                   break_min = as.Date("1996-07-01"),
                                                   origin = 2000L,
                                                   month_start = "Jul",
                                                   width = 1L),
                     seq.Date(from = as.Date("1996-07-01"),
                              to = as.Date("2011-07-01"),
                              by = "1 years"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                                   origin = 2000,
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   width = 5L),
                     seq.Date(from = as.Date("1995-07-01"),
                              to = as.Date("2015-07-01"),
                              by = "5 years"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                                   break_min = NULL,
                                                   origin = 2011,
                                                   month_start = "Jan",
                                                   width = 1L),
                     seq.Date(from = as.Date("1996-01-01"),
                              to = as.Date("2012-01-01"),
                              by = "1 years"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                                   break_min = NULL,
                                                   month_start = "Jan",
                                                   origin = 2000,
                                                   width = 3L),
                     seq.Date(from = as.Date("1994-01-01"),
                              to = as.Date("2012-01-01"),
                              by = "3 years"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                                   break_min = NULL,
                                                   month_start = "Apr",
                                                   origin = 2020,
                                                   width = 10L),
                     seq.Date(from = as.Date("1990-04-01"),
                              to = as.Date("2020-04-01"),
                              by = "10 years"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                                   origin = 1990,
                                                   month_start = "Apr",
                                                   break_min = NULL,
                                                   width = 10L),
                     seq.Date(from = as.Date("1990-04-01"),
                              to = as.Date("2020-04-01"),
                              by = "10 years"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
                                                   break_min = as.Date("1996-04-01"),
                                                   origin = 2000L,
                                                   month_start = "Apr",
                                                   width = 2L),
                     seq.Date(from = as.Date("1996-04-01"),
                              to = as.Date("2012-04-01"),
                              by = "2 years"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(character()),
                                                   origin = 2000L,
                                                   month_start = "Apr",
                                                   width = 2L,
                                                   break_min = NULL),
                     as.Date("2000-04-01"))
    expect_identical(make_breaks_date_to_date_year(date = as.Date(character()),
                                                   origin = 2000L,
                                                   month_start = "Apr",
                                                   width = 2L,
                                                   break_min = as.Date("1996-04-01")),
                     as.Date("1996-04-01"))
})


## make_breaks_date_to_integer_births -----------------------------------------

test_that("'make_breaks_date_to_integer_births' gives correct answer when break_max is finite", {
    expect_identical(make_breaks_date_to_integer_births(age = c(17L, 22L, 37L),
                                                        width = 5L,
                                                        break_min = 15L,
                                                        break_max = 50L),
                     seq.int(from = 15L, by = 5L, to = 50L))
    expect_identical(make_breaks_date_to_integer_births(age = c(17L, 22L, 37L, NA),
                                                        width = 5L,
                                                        break_min = 15L,
                                                        break_max = 50L),
                     seq.int(from = 15L, by = 5L, to = 50L))
    expect_identical(make_breaks_date_to_integer_births(age = c(17L, 22L, 37L, NA),
                                                        width = 5L,
                                                        break_min = NULL,
                                                        break_max = NULL),
                     seq.int(from = 15L, by = 5L, to = 40L))
    expect_identical(make_breaks_date_to_integer_births(age = c(17L, 22L, 37L, NA),
                                                        width = 5L,
                                                        break_min = NULL,
                                                        break_max = 45L),
                     seq.int(from = 15L, by = 5L, to = 45L))
})


## make_breaks_date_to_integer_lifetab ----------------------------------------

test_that("'make_breaks_date_to_integer_lifetab' gives correct answer with valid inputs", {
    expect_identical(make_breaks_date_to_integer_lifetab(age = 0:100,
                                                         break_max = 100L),
                     c(0L, 1L, seq.int(from = 5L, by = 5L, to = 100L)))
    expect_identical(make_breaks_date_to_integer_lifetab(age = 0:4,
                                                         break_max = 5L),
                     c(0L, 1L, 5L))
    expect_identical(make_breaks_date_to_integer_lifetab(age = 0,
                                                         break_max = NULL),
                     c(0L, 1L, 5L))
})


## make_breaks_date_to_integer_month_quarter ----------------------------------

test_that("'make_breaks_date_to_integer_month_quarter' gives correct answer when break_min, break_max both non-NULL", {
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
                                                               break_min = 0L,
                                                               break_max = 100L,
                                                               open_last = TRUE),
                     seq.int(from = 0L, to = 100L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
                                                               break_min = 20L,
                                                               break_max = 100L,
                                                               open_last = TRUE),
                     seq.int(from = 20L, to = 100L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L, NA),
                                                               break_min = 0L,
                                                               break_max = 100L,
                                                               open_last = TRUE),
                     seq.int(from = 0L, to = 100L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 21L),
                                                               break_min = 0L,
                                                               break_max = 100L,
                                                               open_last = FALSE),
                     seq.int(from = 0L, to = 100L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 21L, NA),
                                                               break_min = 0L,
                                                               break_max = 100L,
                                                               open_last = FALSE),
                     seq.int(from = 0L, to = 100L))
})

test_that("'make_breaks_date_to_integer_month_quarter' gives correct answer when break_min is NULL, break_max is non-NULL", {
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
                                                               break_min = NULL,
                                                               break_max = 100,
                                                               open_last = TRUE),
                     seq.int(from = 22L, to = 100L))
})

test_that("'make_breaks_date_to_integer_month_quarter' gives correct answer when break_min is non-NULL, break_max is NULL", {
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = TRUE),
                     seq.int(from = 0L, to = 121L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L, NA),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = TRUE),
                     seq.int(from = 0L, to = 121L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = FALSE),
                     seq.int(from = 0L, to = 122L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L, NA),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = FALSE),
                     seq.int(from = 0L, to = 122L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 80L),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = FALSE),
                     seq.int(from = 0L, to = 81L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 80L, NA),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = FALSE),
                     seq.int(from = 0L, to = 81L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 84L),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = FALSE),
                     seq.int(from = 0L, to = 85L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 84L, NA),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = FALSE),
                     seq.int(from = 0L, to = 85L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 84L, NA),
                                                               break_min = 0L,
                                                               break_max = NULL,
                                                               open_last = TRUE),
                     seq.int(from = 0L, to = 84L))
})

test_that("'make_breaks_date_to_integer_month_quarter' gives correct answer when break_min, break_max both NULL", {
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
                                                               break_min = NULL,
                                                               break_max = NULL,
                                                               open_last = TRUE),
                     seq.int(from = 22L, to = 121L))
    expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
                                                               break_min = NULL,
                                                               break_max = NULL,
                                                               open_last = FALSE),
                     seq.int(from = 22L, to = 122L))
})


## make_breaks_date_to_integer_year -------------------------------------------

test_that("'make_breaks_date_to_integer_year' gives correct answer when break_max is non-NULL", {
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = 100L,
                                                      open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
                                                      width = 5L,
                                                      break_min = 50L,
                                                      break_max = 100L,
                                                      open_last = TRUE),
                     seq.int(from = 50L, by = 5L, to = 100L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L, NA),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = 100L,
                                                      open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 21L),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = 100L,
                                                      open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 21L, NA),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = 100L,
                                                      open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 100L))
})


test_that("'make_breaks_date_to_integer_year' gives correct answer when break_min is NULL", {
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
                                                      width = 5L,
                                                      break_min = NULL,
                                                      break_max = 100L,
                                                      open_last = TRUE),
                     seq.int(from = 20L, by = 5L, to = 100L))
})

test_that("'make_breaks_date_to_integer_year' gives correct answer when break_max is NULL", {
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 120L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L, NA),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 120L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 125L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L, NA),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 125L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 80L),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 80L, NA),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 84L),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 84L, NA),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 84L, NA),
                                                      width = 5L,
                                                      break_min = 0L,
                                                      break_max = NULL,
                                                      open_last = TRUE),
                     seq.int(from = 0L, by = 5L, to = 80L))
})


## make_breaks_labels_to_integer_births --------------------------------------

test_that("'make_breaks_label_to_integer_births' gives correct answer when break_max non-NULL", {
    expect_identical(make_breaks_label_to_integer_births(age_low = c(20L, 22L, 30L, 17L, NA, 43L),
                                                         age_up = c(25L, 25L, 35L, 20L, NA, 44L),
                                                         labels = c("20-24", "22-24", "30-34",
                                                                    "17-19", NA, "43"),
                                                         width = 5L,
                                                         break_min = 15L,
                                                         break_max = 50L),
                     seq.int(15L, 50L, 5L))
})

test_that("'make_breaks_label_to_integer_births' gives correct answer when break_min and break_max NULL", {
    expect_identical(make_breaks_label_to_integer_births(age_low = c(20L, 22L, 30L, 17L, NA, 43L),
                                                         age_up = c(25L, 25L, 35L, 20L, NA, 44L),
                                                         labels = c("20-24", "22-24", "30-34",
                                                                    "17-19", NA, "43"),
                                                         width = 5L,
                                                         break_min = NULL,
                                                         break_max = NULL),
                     seq.int(15L, 45L, 5L))
})


## make_breaks_labels_to_integer_lifetab --------------------------------------

test_that("'make_breaks_label_to_integer_lifetab' gives correct answer when break_max non-NULL", {
    expect_identical(make_breaks_label_to_integer_lifetab(age_low = c(0L, 5L, NA, 10L, 15L),
                                                          age_up = c(1L, 10L, NA, 15L, NA),
                                                          labels = c("0", "5-9", NA, "10-14", "15+"),
                                                          is_open = c(FALSE, FALSE, FALSE, FALSE, TRUE),
                                                          break_max = 15L),
                     c(0L, 1L, 5L, 10L, 15L))
})

test_that("'make_breaks_label_to_integer_lifetab' gives correct answer when break_max NULL", {
    expect_identical(make_breaks_label_to_integer_lifetab(age_low = c(0L, 5L, NA, 10L, 15L),
                                                          age_up = c(1L, 10L, NA, 15L, NA),
                                                          labels = c("0", "5-9", NA, "10-14", "15+"),
                                                          is_open = c(FALSE, FALSE, FALSE, FALSE, TRUE),
                                                          break_max = NULL),
                     c(0L, 1L, 5L, 10L, 15L))
})



## make_breaks_labels_to_integer_month_quarter -------------------------------

test_that("'make_breaks_label_to_integer_month_quarter' gives correct answer when break_min and break_max both non-NULL", {
    expect_identical(make_breaks_label_to_integer_month_quarter(age_low = c(0L, 5L, 10L, 15L),
                                                                age_up = c(1L, 6L, 11L, NA),
                                                                labels = c("0q", "5q", "10q", "15q+"),
                                                                is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                                break_min = 0L,
                                                                break_max = 15L,
                                                                open_last = TRUE),
                     0:15)
    expect_identical(make_breaks_label_to_integer_month_quarter(age_low = c(0L, NA, 10L, 15L),
                                                                age_up = c(1L, NA, 11L, NA),
                                                                labels = c("0q", NA, "10q", "15q+"),
                                                                is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                                break_min = 0L,
                                                                break_max = 15L,
                                                                open_last = TRUE),
                     0:15)
    expect_identical(make_breaks_label_to_integer_month_quarter(age_low = NA,
                                                                age_up = NA,
                                                                labels = NA,
                                                                is_open = FALSE,
                                                                break_min = 0L,
                                                                break_max = 25L,
                                                                open_last = TRUE),
                     0:25)
})

test_that("'make_breaks_label_to_integer_month_quarter' gives correct answer when break_min is NULL and break_max is non-NULL", {
    expect_identical(make_breaks_label_to_integer_month_quarter(age_low = c(0L, 5L, 10L, 15L),
                                                                age_up = c(1L, 6L, 11L, NA),
                                                                labels = c("0q", "5q", "10q", "15q+"),
                                                                is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                                break_min = NULL,
                                                                break_max = 15L,
                                                                open_last = TRUE),
                     0:15)
    expect_identical(make_breaks_label_to_integer_month_quarter(age_low = c(NA, 10L, 15L),
                                                                age_up = c(NA, 11L, NA),
                                                                labels = c(NA, "10q", "15q+"),
                                                                is_open = c(FALSE, FALSE, TRUE),
                                                                break_min = NULL,
                                                                break_max = 15L,
                                                                open_last = TRUE),
                     10:15)
})

test_that("'make_breaks_label_to_integer_month_quarter' gives correct answer when break_min is non-NULL and break_max is NULL", {
    expect_identical(make_breaks_label_to_integer_month_quarter(age_low = c(0L, 5L, 10L, 15L),
                                                       age_up = c(1L, 6L, 11L, NA),
                                                       labels = c("0q", "5q", "10q", "15q+"),
                                                       is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                       break_min = 0L,
                                                       break_max = NULL,
                                                       open_last = TRUE),
                     0:15)
})


## make_breaks_labels_to_integer_year -----------------------------------------

test_that("'make_breaks_label_to_integer_year' gives correct answer when break_min and break_max both non-NULL", {
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, 5L, 10L, 15L),
                                                       age_up = c(5L, 10L, 15L, NA),
                                                       width = 5L,
                                                       labels = c("0-4", "5-9", "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                       break_min = 0L,
                                                       break_max = 15L,
                                                       open_last = TRUE),
                     c(0L, 5L, 10L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, NA, 10L, 15L),
                                                       age_up = c(5L, NA, 15L, NA),
                                                       width = 5L,
                                                       labels = c("0-4", NA, "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                       break_min = 0L,
                                                       break_max = 15L,
                                                       open_last = TRUE),
                     c(0L, 5L, 10L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, NA, 10L, 15L),
                                                       age_up = c(5L, NA, 15L, NA),
                                                       width = 15L,
                                                       labels = c("0-4", "5-9", "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                       break_min = 0L,
                                                       break_max = 15L,
                                                       open_last = TRUE),
                     c(0L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, 5L, 10L, 15L),
                                                       age_up = c(5L, 10L, 15L, 20L),
                                                       width = 5L,
                                                       labels = c("0-4", "5-9", "10-14", "15-19"),
                                                       is_open = c(FALSE, FALSE, FALSE, FALSE),
                                                       break_min = 0L,
                                                       break_max = 25L,
                                                       open_last = TRUE),
                     c(0L, 5L, 10L, 15L, 20L, 25L))
    expect_identical(make_breaks_label_to_integer_year(age_low = NA,
                                                       age_up = NA,
                                                       width = 5L,
                                                       labels = NA,
                                                       is_open = FALSE,
                                                       break_min = 0L,
                                                       break_max = 25L,
                                                       open_last = TRUE),
                     c(0L, 5L, 10L, 15L, 20L, 25L))
})

test_that("'make_breaks_label_to_integer_year' gives correct answer when break_min is NULL and break_max is non-NULL", {
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, 5L, 10L, 15L),
                                                       age_up = c(5L, 10L, 15L, NA),
                                                       width = 5L,
                                                       labels = c("0-4", "5-9", "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                       break_min = NULL,
                                                       break_max = 15L,
                                                       open_last = TRUE),
                     c(0L, 5L, 10L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(NA, 10L, 15L),
                                                       age_up = c(NA, 15L, NA),
                                                       width = 5L,
                                                       labels = c(NA, "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, TRUE),
                                                       break_min = NULL,
                                                       break_max = 15L,
                                                       open_last = TRUE),
                     c(10L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, NA, 10L, 15L),
                                                       age_up = c(5L, NA, 15L, NA),
                                                       width = 15L,
                                                       labels = c("0-4", "5-9", "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                       break_min = NULL,
                                                       break_max = 15L,
                                                       open_last = TRUE),
                     c(0L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, 5L, 10L, 15L),
                                                       age_up = c(5L, 10L, 15L, 20L),
                                                       width = 5L,
                                                       labels = c("0-4", "5-9", "10-14", "15-19"),
                                                       is_open = c(FALSE, FALSE, FALSE, FALSE),
                                                       break_min = NULL,
                                                       break_max = 25L,
                                                       open_last = TRUE),
                     c(0L, 5L, 10L, 15L, 20L, 25L))
})

test_that("'make_breaks_label_to_integer_year' gives correct answer when break_min is non-NULL and break_max is NULL", {
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, 5L, 10L, 15L),
                                                       age_up = c(5L, 10L, 15L, NA),
                                                       width = 5L,
                                                       labels = c("0-4", "5-9", "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                       break_min = 0L,
                                                       break_max = NULL,
                                                       open_last = TRUE),
                     c(0L, 5L, 10L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(NA, 10L, 15L),
                                                       age_up = c(NA, 15L, NA),
                                                       width = 5L,
                                                       labels = c(NA, "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, TRUE),
                                                       break_min = 5L,
                                                       break_max = NULL,
                                                       open_last = TRUE),
                     c(5L, 10L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, NA, 10L, 15L),
                                                       age_up = c(5L, NA, 15L, NA),
                                                       width = 15L,
                                                       labels = c("0-4", "5-9", "10-14", "15+"),
                                                       is_open = c(FALSE, FALSE, FALSE, TRUE),
                                                       break_min = 0L,
                                                       break_max = NULL,
                                                       open_last = TRUE),
                     c(0L, 15L))
    expect_identical(make_breaks_label_to_integer_year(age_low = c(0L, 5L, 10L, 15L),
                                                       age_up = c(5L, 10L, 15L, 20L),
                                                       width = 5L,
                                                       labels = c("0-4", "5-9", "10-14", "15-19"),
                                                       is_open = c(FALSE, FALSE, FALSE, FALSE),
                                                       break_min = 0L,
                                                       break_max = NULL,
                                                       open_last = TRUE),
                     c(0L, 5L, 10L, 15L, 20L))
})




## n_day_month ----------------------------------------------------------------

test_that("'n_day_month' gives correct answer with valid inputs", {
    n_day_month <- demprep:::n_day_month
    expect_identical(n_day_month(as.Date("2000-01-01")), 31L)
    expect_identical(n_day_month(as.Date("2000-02-01")), 29L)
    expect_identical(n_day_month(as.Date("2001-02-01")), 28L)
    expect_identical(n_day_month(as.Date("2000-03-01")), 31L)
    expect_identical(n_day_month(as.Date("2000-04-01")), 30L)
    expect_identical(n_day_month(as.Date("2000-05-01")), 31L)
    expect_identical(n_day_month(as.Date("2000-06-01")), 30L)
    expect_identical(n_day_month(as.Date("2000-07-01")), 31L)
    expect_identical(n_day_month(as.Date("2000-08-01")), 31L)
    expect_identical(n_day_month(as.Date("2000-09-01")), 30L)
    expect_identical(n_day_month(as.Date("2000-10-01")), 31L)
    expect_identical(n_day_month(as.Date("2000-11-01")), 30L)
    expect_identical(n_day_month(as.Date("2000-12-01")), 31L)
})


## rollback_month -------------------------------------------------------------

test_that("'rollback_month' gives correct answer with valid inputs", {
    rollback_month <- demprep:::rollback_month
    expect_identical(rollback_month("2000-01-01"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_month("2000-01-31"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_month("2000-02-29"),
                     as.Date("2000-02-01"))
    expect_identical(rollback_month("2000-12-31"),
                     as.Date("2000-12-01"))
    expect_identical(rollback_month(c("2000-12-31", "2001-01-01")),
                     as.Date(c("2000-12-01", "2001-01-01")))
    expect_identical(rollback_month(as.Date(character())),
                     as.Date(character()))
    expect_identical(rollback_month(as.Date(NA_character_)),
                     as.Date(NA_character_))
})


## rollback_multi -------------------------------------------------------------

test_that("'rollback_multi' gives correct answer with valid inputs", {
    rollback_multi <- demprep:::rollback_multi
    expect_identical(rollback_multi(date = as.Date("2000-01-01"),
                                    width = 5L,
                                    origin = 2000L,
                                    month_start = "Jan"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_multi(date = as.Date("2001-01-01"),
                                    width = 5L,
                                    origin = 2000L,
                                    month_start = "Jan"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_multi(date = as.Date("2000-01-01"),
                                    width = 5L,
                                    origin = 2001L,
                                    month_start = "Jan"),
                     as.Date("1996-01-01"))
    expect_identical(rollback_multi(date = as.Date("2000-01-01"),
                                    width = 5L,
                                    origin = 2001L,
                                    month_start = "Apr"),
                     as.Date("1996-04-01"))
    expect_identical(rollback_multi(date = as.Date("2000-01-20"),
                                    width = 2L,
                                    origin = 2001L,
                                    month_start = "Jun"),
                     as.Date("1999-06-01"))
    expect_identical(rollback_multi(date = as.Date(c("2000-01-20",
                                                     NA,
                                                     "2000-02-29")),
                                    width = 5L,
                                    origin = 2001L,
                                    month_start = "Jun"),
                     as.Date(c("1996-06-01",
                               NA,
                               "1996-06-01")))
    expect_identical(rollback_multi(date = as.Date(c(NA_character_, NA_character_)),
                                    width = 5L,
                                    origin = 2001L,
                                    month_start = "Jun"),
                     as.Date(c(NA_character_, NA_character_)))
})
                                    

## rollback_quarter -----------------------------------------------------------

test_that("'rollback_quarter' gives correct answer with valid inputs", {
    rollback_quarter <- demprep:::rollback_quarter
    expect_identical(rollback_quarter("2000-01-01"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_quarter("2000-01-31"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_quarter("2000-02-29"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_quarter("2000-12-31"),
                     as.Date("2000-10-01"))
    expect_identical(rollback_quarter(c("2000-12-31", "2001-01-01")),
                     as.Date(c("2000-10-01", "2001-01-01")))
    expect_identical(rollback_quarter(as.Date(character())),
                     as.Date(character()))
    expect_identical(rollback_quarter(as.Date(NA_character_)),
                     as.Date(NA_character_))
})


## rollforward_month ----------------------------------------------------------

test_that("'rollforward_month' gives correct answer with valid inputs", {
    rollforward_month <- demprep:::rollforward_month
    expect_identical(rollforward_month("2000-01-01"),
                     as.Date("2000-02-01"))
    expect_identical(rollforward_month("2000-01-31"),
                     as.Date("2000-02-01"))
    expect_identical(rollforward_month("2000-02-29"),
                     as.Date("2000-03-01"))
    expect_identical(rollforward_month("2000-12-31"),
                     as.Date("2001-01-01"))
    expect_identical(rollforward_month(c("2000-12-31", "2001-01-01")),
                     as.Date(c("2001-01-01", "2001-02-01")))
    expect_identical(rollforward_month(as.Date(character())),
                     as.Date(character()))
    expect_identical(rollforward_month(as.Date(NA_character_)),
                     as.Date(NA_character_))
})
