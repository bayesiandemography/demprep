
context("date_to_cohort")

## date_to_cohort_year --------------------------------------------------------

test_that("date_to_cohort_year gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31")),
                     factor(c("2000", "2010", "2004"),
                            levels = 2000:2010))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         break_min = "2000-01-01"),
                     factor(c("2000", "2010", "2004"),
                            levels = c("<2000", 2000:2010)))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         open_left = TRUE),
                     factor(c("2000", "2010", "2004"),
                            levels = c("<2000", 2000:2010)))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         break_min = "1990-01-01"),
                     factor(c("2000", "2010", "2004"),
                            levels = c("<1990", 1990:2010)))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         open_left = TRUE),
                     factor(c("2000", "2010", "2004"),
                            levels = c("<2000", 2000:2010)))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         break_min = "1990-01-01",
                                         as_factor = FALSE),
                     c("2000", "2010", "2004"))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31",
                                                  "1996-03-02"),
                                         break_min = "2000-01-01",
                                         as_factor = FALSE),
                     c("2000", "2010", "2004", "<2000"))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         break_min = "1999-04-01",
                                         label_year_start = FALSE,
                                         open_left = TRUE),
                     factor(c("2000", "2010", "2005"),
                            levels = c("<2000", 2000:2010)))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         break_min = NULL,
                                         month_start = "Apr",
                                         label_year_start = FALSE,
                                         open_left = TRUE),
                     factor(c("2000", "2010", "2005"),
                            levels = c("<2000", 2000:2010)))
})

## date_to_cohort_multi -------------------------------------------------------

test_that("date_to_cohort_year gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31")),
                     factor(c("2000-2005", "2010-2015", "2000-2005"),
                            levels = c("2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_cohort_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31"),
                                          month_start = "Jul",
                                          open_left = TRUE),
                     factor(c("1995-2000", "2005-2010", "2000-2005"),
                            levels = c("<1995", "1995-2000", "2000-2005", "2005-2010")))
    expect_identical(date_to_cohort_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31"),
                                          month_start = "Jul",
                                          origin = 2001),
                     factor(c("1996-2001", "2006-2011", "2001-2006"),
                            levels = c("1996-2001", "2001-2006", "2006-2011")))
    expect_identical(date_to_cohort_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31",
                                                   NA),
                                          month_start = "Jul",
                                          origin = 2001),
                     factor(c("1996-2001", "2006-2011", "2001-2006", NA),
                            levels = c("1996-2001", "2001-2006", "2006-2011")))
    expect_identical(date_to_cohort_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31",
                                                   NA),
                                          break_min = "1996-07-01"),
                     factor(c("1996-2001", "2006-2011", "2001-2006", NA),
                            levels = c("<1996", "1996-2001", "2001-2006", "2006-2011")))
    expect_identical(date_to_cohort_multi(date = character()),
                     factor(integer(), levels = "2000-2005"))
})


## date_to_cohort_quarter -------------------------------------------------------

test_that("date_to_cohort_year gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_quarter(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31")),
                     factor(c("2000 Q1", "2010 Q1", "2004 Q4"),
                            levels = c(paste0(rep(2000:2009, each = 4),
                                              " Q",
                                              1:4),
                                       "2010 Q1")))
    expect_identical(date_to_cohort_quarter(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31"),
                                            break_min = "1999-01-01"),
                     factor(c("2000 Q1", "2010 Q1", "2004 Q4"),
                            levels = c("<1999 Q1",
                                       paste0(rep(1999:2009, each = 4),
                                              " Q",
                                              1:4),
                                       "2010 Q1")))
    expect_identical(date_to_cohort_quarter(date = c(NA, "2004-12-31")),
                     factor(c(NA, "2004 Q4"),
                            levels = "2004 Q4"))
})


## date_to_cohort_month -------------------------------------------------------

test_that("date_to_cohort_year gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_month(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31")),
                     factor(c("2000 Jan", "2010 Jan", "2004 Dec"),
                            levels = c(paste(rep(2000:2009, each = 12),
                                             month.abb),
                                       "2010 Jan")))
    expect_identical(date_to_cohort_month(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31"),
                                          break_min = "1999-01-01"),
                     factor(c("2000 Jan", "2010 Jan", "2004 Dec"),
                            levels = c("<1999 Jan",
                                       paste(rep(1999:2009, each = 12),
                                             month.abb),
                                       "2010 Jan")))
    expect_identical(date_to_cohort_month(date = c(NA, "2004-12-31")),
                     factor(c(NA, "2004 Dec"),
                            levels = c("2004 Dec")))
})



