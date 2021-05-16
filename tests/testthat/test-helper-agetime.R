
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
    expect_identical(age_completed_months(date = as.Date(c(NA,
                                                           "2000-03-01")),
                                          dob = as.Date(c("2000-01-31",
                                                          NA))),
                     c(NA_integer_, NA_integer_))
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


## date_start_month ---------------------------------------------------------

test_that("'date_start_month' gives correct answer with valid inputs", {
    date_start_month <- demprep:::date_start_month
    expect_identical(date_start_month(c("2000 Jan",
                                        "2000 Feb",
                                        "2000 Mar",
                                        "2000 Apr",
                                        "2000 May",
                                        "2000 Jun",
                                        "2000 Jul",
                                        "2000 Aug",
                                        "2000 Sep",
                                        "2000 Oct",
                                        "2000 Nov",
                                        "2000 Dec",
                                        NA,
                                        "2000 Jan+",
                                        "<2000 Jan")),
                     as.Date(c("2000-01-01",
                               "2000-02-01",
                               "2000-03-01",
                               "2000-04-01",
                               "2000-05-01",
                               "2000-06-01",
                               "2000-07-01",
                               "2000-08-01",
                               "2000-09-01",
                               "2000-10-01",
                               "2000-11-01",
                               "2000-12-01",
                               NA,
                               "2000-01-01",
                               "2000-01-01")))
})


## date_start_quarter ---------------------------------------------------------

test_that("'date_start_quarter' gives correct answer with valid inputs", {
    date_start_quarter <- demprep:::date_start_quarter
    expect_identical(date_start_quarter(c("2000 Q1",
                                          "2000 Q2",
                                          "2000 Q3",
                                          "2000 Q4",
                                          NA,
                                          "2000 Q1+",
                                          "2000 Q2+",
                                          "2000 Q3+",
                                          "2000 Q4+",
                                          "<2000 Q1")),
                     as.Date(c("2000-01-01",
                               "2000-04-01",
                               "2000-07-01",
                               "2000-10-01",
                               NA,
                               "2000-01-01",
                               "2000-04-01",
                               "2000-07-01",
                               "2000-10-01",
                               "2000-01-01")))
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

## rollback_year -----------------------------------------------------------

test_that("'rollback_year' gives correct answer with valid inputs", {
    rollback_year <- demprep:::rollback_year
    expect_identical(rollback_year("2000-01-01", month_start = "Jan"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_year("2000-01-31", month_start = "Jan"),
                     as.Date("2000-01-01"))
    expect_identical(rollback_year("2000-02-29", month_start = "Mar"),
                     as.Date("1999-03-01"))
    expect_identical(rollback_year("2000-12-31", month_start = "Aug"),
                     as.Date("2000-08-01"))
    expect_identical(rollback_year(c("2000-12-31", "2001-01-01", NA), month_start = "Feb"),
                     as.Date(c("2000-02-01", "2000-02-01", NA)))
    expect_identical(rollback_year(as.Date(character()), month_start = "Jan"),
                     as.Date(character()))
    expect_identical(rollback_year(as.Date(NA_character_), month_start = "Jan"),
                     as.Date(NA_character_))
})


## rollforward_month ----------------------------------------------------------

test_that("'rollforward_month' gives correct answer with valid inputs", {
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


## rollforward_quarter ----------------------------------------------------------

test_that("'rollforward_quarter' gives correct answer with valid inputs", {
    expect_identical(rollforward_quarter("2000-01-01"),
                     as.Date("2000-04-01"))
    expect_identical(rollforward_quarter("2000-01-31"),
                     as.Date("2000-04-01"))
    expect_identical(rollforward_quarter("2000-02-29"),
                     as.Date("2000-04-01"))
    expect_identical(rollforward_quarter("2000-12-31"),
                     as.Date("2001-01-01"))
    expect_identical(rollforward_quarter(c("2000-12-31", "2001-01-01")),
                     as.Date(c("2001-01-01", "2001-04-01")))
    expect_identical(rollforward_quarter(as.Date(character())),
                     as.Date(character()))
    expect_identical(rollforward_quarter(as.Date(NA_character_)),
                     as.Date(NA_character_))
})
