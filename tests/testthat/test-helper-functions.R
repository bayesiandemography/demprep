
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


## make_age_labels_month_quarter ----------------------------------------------

test_that("'make_age_labels_month_quarter' gives correct answer with valid inputs", {
    expect_identical(make_age_labels_month_quarter(min_break = 0,
                                                   max_break = 5,
                                                   open_left = FALSE,
                                                   open_right = FALSE,
                                                   unit = "month",
                                                   include_na = FALSE),
                     c("0m", "1m", "2m", "3m", "4m"))
    expect_identical(make_age_labels_month_quarter(min_break = 0,
                                                   max_break = 5,
                                                   open_left = FALSE,
                                                   open_right = FALSE,
                                                   unit = "quarter",
                                                   include_na = FALSE),
                     c("0q", "1q", "2q", "3q", "4q"))
    expect_identical(make_age_labels_month_quarter(min_break = 0,
                                                   max_break = 5,
                                                   open_left = TRUE,
                                                   open_right = TRUE,
                                                   unit = "month",
                                                   include_na = FALSE),
                     c("<0m", "0m", "1m", "2m", "3m", "4m", "5m+"))
    expect_identical(make_age_labels_month_quarter(min_break = 0,
                                                   max_break = 5,
                                                   open_left = FALSE,
                                                   open_right = TRUE,
                                                   unit = "quarter",
                                                   include_na = TRUE),
                     c("0q", "1q", "2q", "3q", "4q", "5q+", NA))
})

test_that("'make_age_labels_month_quarter' gives correct error with invalid inputs", {
    expect_error(make_age_labels_month_quarter(min_break = 0,
                                               max_break = 5,
                                               open_left = FALSE,
                                               open_right = FALSE,
                                               unit = "wrong",
                                               include_na = FALSE),
                 "can't handle unit 'wrong'")
})



## make_breaks_integer_lifetab ---------------------------------------------------

test_that("'make_breaks_integer_lifetab' gives correct answer with valid inputs", {
    expect_identical(make_breaks_integer_lifetab(age_max = 100L),
                     c(0L, 1L, seq.int(from = 5L, by = 5L, to = 100L)))
    expect_identical(make_breaks_integer_lifetab(age_max = 5L),
                     c(0L, 1L, 5L))
})


## make_breaks_integer_multi ---------------------------------------------------

test_that("'make_breaks_integer_multi' gives correct answer when age_max is finite", {
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 121L),
                                               width = 5L,
                                               age_max = 100L,
                                               open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 121L, NA),
                                               width = 5L,
                                               age_max = 100L,
                                               open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 21L),
                                               width = 5L,
                                               age_max = 100L,
                                               open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 100L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 21L, NA),
                                               width = 5L,
                                               age_max = 100L,
                                               open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 100L))
})

test_that("'make_breaks_integer_multi' gives correct answer when age_max is Inf", {
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 121L),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 120L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 121L, NA),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 120L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 121L),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 125L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 121L, NA),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 125L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 80L),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 80L, NA),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
      expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 84L),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 84L, NA),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = FALSE),
                     seq.int(from = 0L, by = 5L, to = 85L))
    expect_identical(make_breaks_integer_multi(age = c(50L, 22L, 84L, NA),
                                               width = 5L,
                                               age_max = Inf,
                                               open_right = TRUE),
                     seq.int(from = 0L, by = 5L, to = 80L))
})


## make_breaks_integer_year ---------------------------------------------------

test_that("'make_breaks_integer_year' gives correct answer when age_max is finite", {
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              age_max = 100L,
                                              open_right = TRUE),
                     0:100)
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              age_max = 100L,
                                              open_right = TRUE),
                     0:100)
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 21L),
                                              age_max = 100L,
                                              open_right = FALSE),
                     0:100)
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 21L, NA),
                                              age_max = 100L,
                                              open_right = FALSE),
                     0:100)
})

test_that("'make_breaks_integer_year' gives correct answer when age_max is Inf", {
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              age_max = Inf,
                                              open_right = TRUE),
                     0:121)
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              age_max = Inf,
                                              open_right = TRUE),
                     0:121)
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L),
                                              age_max = Inf,
                                              open_right = FALSE),
                     0:122)
    expect_identical(make_breaks_integer_year(age = c(50L, 22L, 121L, NA),
                                              age_max = Inf,
                                              open_right = FALSE),
                     0:122)
})







                     



