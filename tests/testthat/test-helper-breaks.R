
context("helper-breaks")

## make_break_min_pairs_date --------------------------------------------------

test_that("'make_break_min_pairs_date' gives correct answer with valid input", {
    expect_identical(make_break_min_pairs_date(pairs = list(as.Date(c("2001-01-01", "2001-04-01")),
                                                      as.Date(c("2001-10-01", "2002-01-01")),
                                                      as.Date(c(NA, NA)))),
                     as.Date("2001-01-01"))
    expect_identical(make_break_min_pairs_date(pairs = list(as.Date(c("2001-01-01", "2001-04-01")),
                                                      as.Date(c("2001-10-01", "2002-01-01")),
                                                      as.Date(c(NA, "2001-07-01")))),
                     as.Date("2001-07-01"))
})


## make_break_max_pairs_date --------------------------------------------------

test_that("'make_break_max_pairs_date' gives correct answer with valid input", {
    expect_identical(make_break_max_pairs_date(pairs = list(as.Date(c("2001-01-01", "2001-04-01")),
                                                      as.Date(c("2001-10-01", "2002-01-01")),
                                                      as.Date(c(NA, NA)))),
                     as.Date("2002-01-01"))
    expect_identical(make_break_max_pairs_date(pairs = list(as.Date(c("2001-01-01", "2001-04-01")),
                                                      as.Date(c("2001-10-01", "2002-01-01")),
                                                      as.Date(c("2001-07-01", NA)))),
                     as.Date("2001-07-01"))
})

                                           
## ## make_breaks_date_to_date_month ---------------------------------------------

## test_that("'make_breaks_date_to_date_month' gives correct answer with valid input", {
##     expect_identical(make_breaks_date_to_date_month(date = as.Date("2001-01-05"),
##                                                     break_min = NULL,
##                                                     has_break_min_arg = FALSE),
##                      as.Date(c("2001-01-01", "2001-02-01")))
##     expect_identical(make_breaks_date_to_date_month(date = as.Date(c("2001-01-05", "2000-02-29")),
##                                                     break_min = NULL,
##                                                     has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("2000-02-01"),
##                               to = as.Date("2001-02-01"),
##                               by = "month"))
##     expect_identical(make_breaks_date_to_date_month(date = as.Date(c("2001-01-05", NA, "2000-02-29")),
##                                                     break_min = NULL,
##                                                     has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("2000-02-01"),
##                               to = as.Date("2001-02-01"),
##                               by = "month"))
##     expect_identical(make_breaks_date_to_date_month(date = as.Date(c("2001-01-05", NA, "2005-12-29")),
##                                                     break_min = NULL,
##                                                     has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("2001-01-01"),
##                               to = as.Date("2006-01-01"),
##                               by = "month"))
##     expect_identical(make_breaks_date_to_date_month(date = as.Date("2001-01-05"),
##                                                     break_min = as.Date("2000-01-01"),
##                                                     has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("2000-01-01"),
##                               to = as.Date("2001-02-01"),
##                               by = "month"))
##     expect_identical(make_breaks_date_to_date_month(date = as.Date(c("2001-01-05", "2000-02-29")),
##                                                     break_min = as.Date("1999-12-01"),
##                                                     has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("1999-12-01"),
##                               to = as.Date("2001-02-01"),
##                               by = "month"))
##     expect_identical(make_breaks_date_to_date_month(date = as.Date(c(NA, NA)),
##                                                     break_min = as.Date("1999-12-01"),
##                                                     has_break_min_arg = FALSE),
##                      as.Date("1999-12-01"))
##     expect_identical(make_breaks_date_to_date_month(date = as.Date(character()),
##                                                     break_min = as.Date("1999-12-01"),
##                                                     has_break_min_arg = FALSE),
##                      as.Date("1999-12-01"))
## })


## ## make_breaks_date_to_date_quarter -------------------------------------------

## test_that("'make_breaks_date_to_date_quarter' gives correct answer with valid input", {
##     expect_identical(make_breaks_date_to_date_quarter(date = as.Date("2001-01-05"),
##                                                       break_min = NULL,
##                                                       has_break_min_arg = FALSE),
##                      as.Date(c("2001-01-01", "2001-04-01")))
##     expect_identical(make_breaks_date_to_date_quarter(date = as.Date(c("2001-01-05", "2000-02-29")),
##                                                       break_min = NULL,
##                                                       has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("2000-01-01"),
##                               to = as.Date("2001-04-01"),
##                               by = "quarter"))
##     expect_identical(make_breaks_date_to_date_quarter(date = as.Date(c("2001-01-05", NA, "2000-02-29")),
##                                                       break_min = NULL,
##                                                       has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("2000-01-01"),
##                               to = as.Date("2001-04-01"),
##                               by = "quarter"))
##     expect_identical(make_breaks_date_to_date_quarter(date = as.Date(c("2001-01-05", NA, "2005-12-29")),
##                                                       break_min = NULL,
##                                                       has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("2001-01-01"),
##                               to = as.Date("2006-01-01"),
##                               by = "quarter"))
##     expect_identical(make_breaks_date_to_date_quarter(date = as.Date("2001-01-05"),
##                                                       break_min = as.Date("2000-01-01"),
##                                                       has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("2000-01-01"),
##                               to = as.Date("2001-04-01"),
##                               by = "quarter"))
##     expect_identical(make_breaks_date_to_date_quarter(date = as.Date(c("2001-01-05", "2000-02-29")),
##                                                       break_min = as.Date("1999-10-01"),
##                                                       has_break_min_arg = FALSE),
##                      seq.Date(from = as.Date("1999-10-01"),
##                               to = as.Date("2001-04-01"),
##                               by = "quarter"))
## })


## ## make_breaks_date_to_date_year ----------------------------------------------

## test_that("'make_breaks_date_to_date_year' gives correct answer with valid input", {
##     expect_identical(make_breaks_date_to_date_year(date = as.Date("2001-01-05"),
##                                                    month_start = "Jan"),
##                      seq.Date(from = as.Date("2001-01-01"),
##                               to = as.Date("2002-01-01"),
##                               by = "1 year"))
##     expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-08-03", "2010-12-31")),
##                                                    month_start = "Jul"),
##                      seq.Date(from = as.Date("1996-07-01"),
##                               to = as.Date("2011-07-01"),
##                               by = "1 year"))
##     expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03",
##                                                                     "2011-05-23",
##                                                                     "2001-01-01", NA)),
##                                                    month_start = "Jul"),
##                      seq.Date(from = as.Date("1996-07-01"),
##                               to = as.Date("2011-07-01"),
##                               by = "year"))
##     expect_identical(make_breaks_date_to_date_year(date = as.Date(c("1996-07-03", "2011-05-23", "2001-01-01", NA)),
##                                                    month_start = "Jan"),
##                      seq.Date(from = as.Date("1996-01-01"),
##                               to = as.Date("2012-01-01"),
##                               by = "year"))
## })


## ## make_breaks_date_to_integer_births -----------------------------------------

## test_that("'make_breaks_date_to_integer_births' gives correct answer when break_max is finite", {
##     expect_identical(make_breaks_date_to_integer_births(age = c(17L, 22L, 37L),
##                                                         width = 5L,
##                                                         break_min = 15L,
##                                                         break_max = 50L),
##                      seq.int(from = 15L, by = 5L, to = 50L))
##     expect_identical(make_breaks_date_to_integer_births(age = c(17L, 22L, 37L, NA),
##                                                         width = 5L,
##                                                         break_min = 15L,
##                                                         break_max = 50L),
##                      seq.int(from = 15L, by = 5L, to = 50L))
##     expect_identical(make_breaks_date_to_integer_births(age = c(17L, 22L, 37L, NA),
##                                                         width = 5L,
##                                                         break_min = NULL,
##                                                         break_max = NULL),
##                      seq.int(from = 15L, by = 5L, to = 40L))
##     expect_identical(make_breaks_date_to_integer_births(age = c(17L, 22L, 37L, NA),
##                                                         width = 5L,
##                                                         break_min = NULL,
##                                                         break_max = 45L),
##                      seq.int(from = 15L, by = 5L, to = 45L))
## })


## ## make_breaks_date_to_integer_lifetab ----------------------------------------

## test_that("'make_breaks_date_to_integer_lifetab' gives correct answer with valid inputs", {
##     expect_identical(make_breaks_date_to_integer_lifetab(age = 0:100,
##                                                          break_max = 100L),
##                      c(0L, 1L, seq.int(from = 5L, by = 5L, to = 100L)))
##     expect_identical(make_breaks_date_to_integer_lifetab(age = 0:4,
##                                                          break_max = 5L),
##                      c(0L, 1L, 5L))
##     expect_identical(make_breaks_date_to_integer_lifetab(age = 0,
##                                                          break_max = NULL),
##                      c(0L, 1L, 5L))
## })


## ## make_breaks_date_to_integer_month_quarter ----------------------------------

## test_that("'make_breaks_date_to_integer_month_quarter' gives correct answer when break_min, break_max both non-NULL", {
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
##                                                                break_min = 0L,
##                                                                break_max = 100L,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 0L, to = 100L))
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
##                                                                break_min = 20L,
##                                                                break_max = 100L,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 20L, to = 100L))
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L, NA),
##                                                                break_min = 0L,
##                                                                break_max = 100L,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 0L, to = 100L))
## })

## test_that("'make_breaks_date_to_integer_month_quarter' gives correct answer when break_min is NULL, break_max is non-NULL", {
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
##                                                                break_min = NULL,
##                                                                break_max = 100,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 22L, to = 100L))
## })

## test_that("'make_breaks_date_to_integer_month_quarter' gives correct answer when break_min is non-NULL, break_max is NULL", {
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
##                                                                break_min = 0L,
##                                                                break_max = NULL,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 0L, to = 122L))
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L, NA),
##                                                                break_min = 0L,
##                                                                break_max = NULL,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 0L, to = 122L))
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 80L),
##                                                                break_min = 0L,
##                                                                break_max = NULL,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 0L, to = 81L))
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 80L, NA),
##                                                                break_min = 0L,
##                                                                break_max = NULL,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 0L, to = 81L))
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 84L),
##                                                                break_min = 0L,
##                                                                break_max = NULL,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 0L, to = 85L))
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 84L, NA),
##                                                                break_min = 0L,
##                                                                break_max = NULL,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 0L, to = 85L))
## })

## test_that("'make_breaks_date_to_integer_month_quarter' gives correct answer when break_min, break_max both NULL", {
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
##                                                                break_min = NULL,
##                                                                break_max = NULL,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 22L, to = 122L))
##     expect_identical(make_breaks_date_to_integer_month_quarter(age = c(50L, 22L, 121L),
##                                                                break_min = NULL,
##                                                                break_max = NULL,
##                                                                has_break_min_arg = FALSE,
##                                                                has_break_max_arg = FALSE),
##                      seq.int(from = 22L, to = 122L))
## })


## ## make_breaks_date_to_integer_year -------------------------------------------

## test_that("'make_breaks_date_to_integer_year' gives correct answer when break_max is non-NULL", {
##     make_breaks_date_to_integer_year <- demprep:::make_breaks_date_to_integer_year
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
##                                                       width = 5L,
##                                                       break_min = 0L,
##                                                       break_max = 100L,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 0L, by = 5L, to = 100L))
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
##                                                       width = 5L,
##                                                       break_min = 50L,
##                                                       break_max = 100L,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 50L, by = 5L, to = 100L))
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L, NA),
##                                                       width = 5L,
##                                                       break_min = 0L,
##                                                       break_max = 100L,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 0L, by = 5L, to = 100L))
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 21L),
##                                                       width = 5L,
##                                                       break_min = 0L,
##                                                       break_max = 100L,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 0L, by = 5L, to = 100L))
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 21L, NA),
##                                                       width = 5L,
##                                                       break_min = 0L,
##                                                       break_max = 100L,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 0L, by = 5L, to = 100L))
## })


## test_that("'make_breaks_date_to_integer_year' gives correct answer when break_min is NULL", {
##     make_breaks_date_to_integer_year <- demprep:::make_breaks_date_to_integer_year
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
##                                                       width = 5L,
##                                                       break_min = NULL,
##                                                       break_max = 100L,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 20L, by = 5L, to = 100L))
## })

## test_that("'make_breaks_date_to_integer_year' gives correct answer when break_max is NULL", {
##     make_breaks_date_to_integer_year <- demprep:::make_breaks_date_to_integer_year
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L),
##                                                       width = 5L,
##                                                       break_min = 0L,
##                                                       break_max = NULL,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 0L, by = 5L, to = 125L))
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 121L, NA),
##                                                       width = 5L,
##                                                       break_min = 0L,
##                                                       break_max = NULL,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 0L, by = 5L, to = 125L))
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 80L),
##                                                       width = 5L,
##                                                       break_min = 0L,
##                                                       break_max = NULL,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 0L, by = 5L, to = 85L))
##     expect_identical(make_breaks_date_to_integer_year(age = c(50L, 22L, 84L),
##                                                       width = 5L,
##                                                       break_min = 0L,
##                                                       break_max = NULL,
##                                                       has_break_min_arg = FALSE,
##                                                       has_break_max_arg = FALSE),
##                      seq.int(from = 0L, by = 5L, to = 85L))
## })


## ## make_breaks_labels_to_date_month_quarter -----------------------------------------

## test_that("'make_breaks_label_to_date_month_quarter' gives correct answer with 'break_min' equal to NULL", {
##     make_breaks_label_to_date_month_quarter <- demprep:::make_breaks_label_to_date_month_quarter
##     expect_identical(make_breaks_label_to_date_month_quarter(date_low = as.Date(c("2000-01-01",
##                                                                                   "2002-10-01",
##                                                                                   "2001-04-01")),
##                                                              date_up = as.Date(c("2000-04-01",
##                                                                                  "2003-01-01",
##                                                                                  "2001-07-01")),
##                                                              break_min = NULL,
##                                                              has_break_min_arg = FALSE,
##                                                              is_open = c(FALSE,
##                                                                          FALSE,
##                                                                          FALSE),
##                                                              unit = "quarter"),
##                      seq.Date(from = as.Date("2000-01-01"),
##                               to = as.Date("2003-01-01"),
##                               by = "quarter"))
##     expect_identical(make_breaks_label_to_date_month_quarter(date_low = as.Date(c("2000-01-01",
##                                                                                   "2002-10-01",
##                                                                                   "2001-04-01")),
##                                                              date_up = as.Date(c("2000-02-01",
##                                                                                  "2002-11-01",
##                                                                                  "2001-05-01")),
##                                                              break_min = NULL,
##                                                              has_break_min_arg = FALSE,
##                                                              is_open = c(FALSE,
##                                                                          FALSE,
##                                                                          FALSE),
##                                                              unit = "month"),
##                      seq.Date(from = as.Date("2000-01-01"),
##                               to = as.Date("2002-11-01"),
##                               by = "month"))
##     expect_identical(make_breaks_label_to_date_month_quarter(date_low = as.Date(c("2000-01-01",
##                                                                                   NA,
##                                                                                   "2001-04-01")),
##                                                              date_up = as.Date(c("2000-04-01",
##                                                                                  "2003-01-01",
##                                                                                  "2001-07-01")),
##                                                              break_min = NULL,
##                                                              has_break_min_arg = FALSE,
##                                                              is_open = c(FALSE,
##                                                                          TRUE,
##                                                                          FALSE),
##                                                              unit = "quarter"),
##                      as.Date("2003-01-01"))
##     expect_identical(make_breaks_label_to_date_month_quarter(date_low = as.Date(c("2000-01-01",
##                                                                                   "2002-10-01",
##                                                                                   NA)),
##                                                              date_up = as.Date(c("2000-04-01",
##                                                                                  "2003-01-01",
##                                                                                  "2001-07-01")),
##                                                              break_min = NULL,
##                                                              has_break_min_arg = FALSE,
##                                                              is_open = c(FALSE,
##                                                                          FALSE,
##                                                                          TRUE),
##                                                              unit = "quarter"),
##                      seq.Date(from = as.Date("2001-07-01"),
##                               to = as.Date("2003-01-01"),
##                               by = "quarter"))
## })

## test_that("'make_breaks_label_to_date_month_quarter' gives correct answer with 'break_min' non-NULL", {
##     make_breaks_label_to_date_month_quarter <- demprep:::make_breaks_label_to_date_month_quarter
##     expect_identical(make_breaks_label_to_date_month_quarter(date_low = as.Date(c("2000-01-01",
##                                                                                   "2002-10-01",
##                                                                                   "2001-04-01")),
##                                                              date_up = as.Date(c("2000-04-01",
##                                                                                  "2003-01-01",
##                                                                                  "2001-07-01")),
##                                                              break_min = as.Date("1999-10-01"),
##                                                              has_break_min_arg = FALSE,
##                                                              is_open = c(FALSE,
##                                                                          FALSE,
##                                                                          FALSE),
##                                                              unit = "quarter"),
##                      seq.Date(from = as.Date("1999-10-01"),
##                               to = as.Date("2003-01-01"),
##                               by = "quarter"))
##     expect_identical(make_breaks_label_to_date_month_quarter(date_low = as.Date(c("2000-01-01",
##                                                                                   NA,
##                                                                                   "2001-04-01")),
##                                                              date_up = as.Date(c("2000-04-01",
##                                                                                  "2003-01-01",
##                                                                                  "2001-07-01")),
##                                                              break_min = as.Date("2003-01-01"),
##                                                              has_break_min_arg = FALSE,
##                                                              is_open = c(FALSE,
##                                                                          TRUE,
##                                                                          FALSE),
##                                                              unit = "quarter"),
##                      as.Date("2003-01-01"))
##     expect_identical(make_breaks_label_to_date_month_quarter(date_low = as.Date(c("2000-01-01",
##                                                                                   "2002-10-01",
##                                                                                   NA)),
##                                                              date_up = as.Date(c("2000-04-01",
##                                                                                  "2003-01-01",
##                                                                                  "2001-07-01")),
##                                                              break_min = NULL,
##                                                              has_break_min_arg = FALSE,
##                                                              is_open = c(FALSE,
##                                                                          FALSE,
##                                                                          TRUE),
##                                                              unit = "quarter"),
##                      seq.Date(from = as.Date("2001-07-01"),
##                               to = as.Date("2003-01-01"),
##                               by = "quarter"))
## })



## ## make_breaks_labels_to_integer_births --------------------------------------

## test_that("'make_breaks_label_to_integer_births' gives correct answer when break_max non-NULL", {
##     expect_identical(make_breaks_label_to_integer_births(age_low = c(20L, 22L, 30L, 17L, NA, 43L),
##                                                          age_up = c(25L, 25L, 35L, 20L, NA, 44L),
##                                                          labels = c("20-24", "22-24", "30-34",
##                                                                     "17-19", NA, "43"),
##                                                          width = 5L,
##                                                          break_min = 15L,
##                                                          break_max = 50L),
##                      seq.int(15L, 50L, 5L))
## })

## test_that("'make_breaks_label_to_integer_births' gives correct answer when break_min and break_max NULL", {
##     expect_identical(make_breaks_label_to_integer_births(age_low = c(20L, 22L, 30L, 17L, NA, 43L),
##                                                          age_up = c(25L, 25L, 35L, 20L, NA, 44L),
##                                                          labels = c("20-24", "22-24", "30-34",
##                                                                     "17-19", NA, "43"),
##                                                          width = 5L,
##                                                          break_min = NULL,
##                                                          break_max = NULL),
##                      seq.int(15L, 45L, 5L))
## })


## ## make_breaks_labels_to_integer_lifetab --------------------------------------

## test_that("'make_breaks_label_to_integer_lifetab' gives correct answer when break_max non-NULL", {
##     expect_identical(make_breaks_label_to_integer_lifetab(age_low = c(0L, 5L, NA, 10L, 15L),
##                                                           age_up = c(1L, 10L, NA, 15L, NA),
##                                                           labels = c("0", "5-9", NA, "10-14", "15+"),
##                                                           is_open = c(FALSE, FALSE, FALSE, FALSE, TRUE),
##                                                           break_max = 15L),
##                      c(0L, 1L, 5L, 10L, 15L))
## })

## test_that("'make_breaks_label_to_integer_lifetab' gives correct answer when break_max NULL", {
##     expect_identical(make_breaks_label_to_integer_lifetab(age_low = c(0L, 5L, NA, 10L, 15L),
##                                                           age_up = c(1L, 10L, NA, 15L, NA),
##                                                           labels = c("0", "5-9", NA, "10-14", "15+"),
##                                                           is_open = c(FALSE, FALSE, FALSE, FALSE, TRUE),
##                                                           break_max = NULL),
##                      c(0L, 1L, 5L, 10L, 15L))
## })


## ## make_breaks_label_to_integer -----------------------------------------------

## test_that("'make_breaks_label_to_integer' gives correct answer when break_min and break_max both non-NULL", {
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, 15L),
##                                                   int_up = c(5L, 10L, 15L, NA),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, 15L),
##                                                   int_up = c(5L, 10L, 15L, NA),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "<0"),
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, NA, 10L, 15L),
##                                                   int_up = c(5L, NA, 15L, NA),
##                                                   width = 5L,
##                                                   labels = c("0-4", NA, "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, NA, 10L, 15L),
##                                                   int_up = c(5L, NA, 15L, NA),
##                                                   width = 15L,
##                                                   labels = c("0-4", "5-9", "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, 15L),
##                                                   int_up = c(5L, 10L, 15L, 20L),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "15-19"),
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = 25L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L, 20L, 25L))
##     expect_identical(make_breaks_label_to_integer(int_low = NA,
##                                                   int_up = NA,
##                                                   width = 5L,
##                                                   labels = NA,
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = 25L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L, 20L, 25L))
## })

## test_that("'make_breaks_label_to_integer' gives correct answer when break_min is NULL and break_max is non-NULL", {
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, 15L),
##                                                   int_up = c(5L, 10L, 15L, NA),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = NULL,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, NA),
##                                                   int_up = c(5L, 10L, 15L, 1L),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "<1"),
##                                                   origin = 0L,
##                                                   break_min = NULL,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(5L, 10L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(NA, 10L, 15L),
##                                                   int_up = c(NA, 15L, NA),
##                                                   width = 5L,
##                                                   labels = c(NA, "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = NULL,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(10L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, NA, 10L, 15L),
##                                                   int_up = c(5L, NA, 15L, NA),
##                                                   width = 15L,
##                                                   labels = c("0-4", "5-9", "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = NULL,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, 15L),
##                                                   int_up = c(5L, 10L, 15L, 20L),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "15-19"),
##                                                   origin = 0L,
##                                                   break_min = NULL,
##                                                   break_max = 25L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L, 20L, 25L))
## })

## test_that("'make_breaks_label_to_integer' gives correct answer when break_min is non-NULL and break_max is NULL", {
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, 15L),
##                                                   int_up = c(5L, 10L, 15L, NA),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = NULL,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(NA, 10L, 15L),
##                                                   int_up = c(NA, 15L, NA),
##                                                   width = 5L,
##                                                   labels = c(NA, "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = 5L,
##                                                   break_max = NULL,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(5L, 10L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, NA, 10L, 15L),
##                                                   int_up = c(5L, NA, 15L, NA),
##                                                   width = 15L,
##                                                   labels = c("0-4", "5-9", "10-14", "15+"),
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = NULL,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 15L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, 15L),
##                                                   int_up = c(5L, 10L, 15L, 20L),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "15-19"),
##                                                   origin = 0L,
##                                                   break_min = 0L,
##                                                   break_max = NULL,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(0L, 5L, 10L, 15L, 20L))
## })

## test_that("'make_breaks_label_to_integer' gives correct answer when origin is not 0", {
##     expect_identical(make_breaks_label_to_integer(int_low = c(2001L, 2006L, NA),
##                                                   int_up = c(2006L, 2011L, NA),
##                                                   width = 5L,
##                                                   labels = c("2001-2006", "2006-2011", NA),
##                                                   origin = 2001L,
##                                                   break_min = NULL,
##                                                   break_max = NULL,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(2001L, 2006L, 2011L))
##     expect_identical(make_breaks_label_to_integer(int_low = c(0L, 5L, 10L, NA),
##                                                   int_up = c(5L, 10L, 15L, 1L),
##                                                   width = 5L,
##                                                   labels = c("0-4", "5-9", "10-14", "<1"),
##                                                   origin = -5L,
##                                                   break_min = NULL,
##                                                   break_max = 15L,
##                                                   has_break_min_arg = FALSE,
##                                                   has_break_max_arg = FALSE),
##                      c(5L, 10L, 15L))
## })

