
context("date_to_period")

## date_to_period_year --------------------------------------------------

test_that("date_to_period_year gives correct answers with valid inputs", {
    expect_identical(date_to_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         month_start = "Jan"),
                     factor(c("2000", "2010", "2004"),
                            levels = 2000:2010))
    expect_identical(date_to_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         month_start = "Jun"),
                     factor(c("1999", "2009", "2004"),
                            levels = 1999:2009))
    expect_identical(date_to_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         month_start = "Jan",
                                         as_factor = FALSE),
                     c("2000", "2010", "2004"))
    expect_identical(date_to_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         label_year_start = FALSE,
                                         month_start = "Apr"),
                     factor(c("2000", "2010", "2005"),
                            levels = 2000:2010))
    expect_identical(date_to_period_year(date = c("2003-03-20",
                                                  "2001-02-11",
                                                  "2004-12-30"),
                                         label_year_start = TRUE,
                                         month_start = "Jan",
                                         as_factor = TRUE),
                     factor(c("2003", "2001", "2004"),
                            levels = as.character(2001:2004)))
    expect_identical(date_to_period_year(date = c("2003-03-20",
                                                  "2001-02-11",
                                                  "2004-12-30"),
                                         label_year_start = TRUE,
                                         month_start = "Jul",
                                         as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = as.character(2000:2004)))
    expect_identical(date_to_period_year(date = c("2003-03-20",
                                                  "2001-02-11",
                                                  "2004-12-30"),
                                         label_year_start = TRUE,
                                         month_start = "Jul",
                                         as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = as.character(2000:2004)))
    expect_identical(date_to_period_year(date = c("2003-03-20",
                                                  "2001-02-11",
                                                  "2004-12-30"),
                                         label_year_start = FALSE,
                                         month_start = "Jul",
                                         as_factor = TRUE),
                     factor(c("2003", "2001", "2005"),
                            levels = as.character(2001:2005)))
})


## date_to_period_multi --------------------------------------------------

test_that("date_to_period_multi gives correct answers with valid inputs", {
    expect_identical(date_to_period_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31")),
                     factor(c("2000-2005", "2010-2015", "2000-2005"),
                            levels = c("2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_period_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   NA)),
                     factor(c("2000-2005", "2010-2015", NA),
                            levels = c("2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_period_multi(date = character()),
                     factor(character(),
                            levels = "2000-2005"))
})


## date_to_period_custom --------------------------------------------------

test_that("date_to_period_custom gives correct answers with valid inputs", {
    expect_identical(date_to_period_custom(date = c("2000-01-01",
                                                    "2010-01-01",
                                                    "2004-12-31"),
                                           breaks = c("2000-01-01",
                                                      "2008-01-01",
                                                      "2015-01-01")),
                     factor(c("2000-2008", "2008-2015", "2000-2008"),
                            levels = c("2000-2008", "2008-2015")))
    expect_identical(date_to_period_custom(date = c("2000-01-01",
                                                    "2010-01-01",
                                                    NA,
                                                    "2004-12-31"),
                                           breaks = c("1991-07-01",
                                                      "2008-07-01",
                                                      "2015-07-01")),
                     factor(c("1991-2008", "2008-2015", NA, "1991-2008"),
                            levels = c("1991-2008", "2008-2015")))
    expect_identical(date_to_period_custom(date = character(),
                                           breaks = c("2000-03-01",
                                                      "2005-03-01")),
                     factor(character(),
                            levels = "2000-2005"))
    expect_identical(date_to_period_custom(date = c("2003-03-20",
                                                    "2001-02-11",
                                                    "2010-12-30"),
                                           breaks = c("2000-01-01",
                                                      "2006-01-01",
                                                      "2020-01-01"),
                                           as_factor = TRUE),
                     factor(c("2000-2006", "2000-2006", "2006-2020"),
                            levels = c("2000-2006", "2006-2020")))
    expect_identical(date_to_period_custom(date = c("2003-03-20",
                                                    "2001-02-11",
                                                    NA,
                                                    "2010-12-30",
                                                    "1999-03-02"),
                                           breaks = c("1999-03-01",
                                                      "2000-03-01",
                                                      "2006-03-01",
                                                      "2020-03-01"),
                                           as_factor = TRUE),
                     factor(c("2000-2006", "2000-2006", NA, "2006-2020", "1999-2000"),
                            levels = c("1999-2000", "2000-2006", "2006-2020")))
    expect_identical(date_to_period_custom(date = c(NA, NA),
                                           breaks = c("2001-04-01",
                                                      "2003-04-01")),
                     factor(c(NA, NA),
                            levels = "2001-2003"))
    expect_identical(date_to_period_custom(date = character(),
                                            breaks = c("2001-04-01",
                                                       "2003-04-01")),
                     factor(character(), levels = "2001-2003"))
    expect_identical(date_to_period_custom(date = character(),
                                           breaks = c("2001-04-01",
                                                      "2003-04-01"),
                                           as_factor = FALSE),
                     character())
    expect_identical(date_to_period_custom(date = character(),
                                           breaks = character(),
                                           as_factor = FALSE),
                     character())
})

test_that("date_to_period_custom throws expected error with invalid inputs", {
    expect_error(date_to_period_custom(date = "2000-01-01",
                                       breaks = character(),
                                       as_factor = FALSE),
                 "'breaks' has length 0")
})


## date_to_period_quarter ---------------------------------------------------

test_that("date_to_period_quarter gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_quarter(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31")),
                     factor(c("2000 Q1", "2010 Q1", "2004 Q4"),
                            levels = c(paste0(rep(2000:2009, each = 4),
                                              " Q",
                                              1:4),
                                       "2010 Q1")))
    expect_identical(date_to_cohort_quarter(date = c(NA, "2004-12-31")),
                     factor(c(NA, "2004 Q4"),
                            levels = "2004 Q4"))
    expect_identical(date_to_cohort_quarter(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                            as_factor = TRUE),
                     factor(c("2003 Q1", "2001 Q1", "2010 Q4"),
                            levels = paste0(rep(2001:2010, each = 4),
                                            " Q",
                                            1:4)))
    expect_identical(date_to_cohort_quarter(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                            as_factor = FALSE),
                     c("2003 Q1", "2001 Q1", "2010 Q4"))
    expect_identical(date_to_cohort_quarter(date = c("2000-01-01", "2000-01-01"),
                                            as_factor = TRUE),
                     factor(c("2000 Q1", "2000 Q1"),
                            levels = "2000 Q1"))
    expect_identical(date_to_period_quarter(date = c("2000-01-01",
                                                     "2000-05-11",
                                                     NA,
                                                     "2001-02-28")),
                     factor(c("2000 Q1", "2000 Q2", NA,  "2001 Q1"),
                            levels = c("2000 Q1", "2000 Q2", "2000 Q3", "2000 Q4",
                                       "2001 Q1")))
    expect_identical(date_to_period_quarter(date = "2000-01-01"),
                     factor("2000 Q1", levels = "2000 Q1"))
    expect_identical(date_to_period_quarter(date = "2000-01-01",
                                            as_factor = FALSE),
                     "2000 Q1")
    expect_identical(date_to_period_quarter(date = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = character()))
    expect_identical(date_to_period_quarter(date = character(),
                                            as_factor = FALSE),
                     character())
})


## date_to_period_month ---------------------------------------------------

test_that("date_to_period_month gives correct answers with valid inputs", {
    expect_identical(date_to_period_month(date = c("2000-01-01",
                                                   "2000-05-11",
                                                   NA,
                                                   "2001-02-28")),
                     factor(c("2000 Jan", "2000 May", NA,  "2001 Feb"),
                            levels = c(paste("2000", month.abb),
                                       "2001 Jan", "2001 Feb")))
    expect_identical(date_to_period_month(date = "2000-01-01"),
                     factor("2000 Jan", levels = "2000 Jan"))
    expect_identical(date_to_period_month(date = "2000-01-01",
                                          as_factor = FALSE),
                     "2000 Jan")
    expect_identical(date_to_period_month(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31")),
                     factor(c("2000 Jan", "2010 Jan", "2004 Dec"),
                            levels = c(paste(rep(2000:2009, each = 12),
                                             month.abb),
                                       "2010 Jan")))
    expect_identical(date_to_period_month(date = c(NA, "2004-12-31")),
                     factor(c(NA, "2004 Dec"),
                            levels = c("2004 Dec")))
    expect_identical(date_to_period_month(date = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = character()))
    expect_identical(date_to_period_month(date = character(),
                                          as_factor = FALSE),
                     character())
    expect_identical(date_to_period_month(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                          as_factor = TRUE),
                     factor(c("2003 Mar", "2001 Feb", "2010 Dec"),
                            levels = paste(rep(2001:2010, each = 12), month.abb)[-1]))
    expect_identical(date_to_period_month(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                          as_factor = FALSE),
                     c("2003 Mar", "2001 Feb", "2010 Dec"))
    expect_identical(date_to_period_month(date = c("2000-01-01", "2000-01-01"),
                                          as_factor = TRUE),
                     factor(c("2000 Jan", "2000 Jan"),
                            levels = "2000 Jan"))
    expect_identical(date_to_period_month(date = c("2000-01-01", NA, "2000-01-01"),
                                          as_factor = TRUE),
                     factor(c("2000 Jan", NA, "2000 Jan"),
                            levels = c("2000 Jan")))
})





