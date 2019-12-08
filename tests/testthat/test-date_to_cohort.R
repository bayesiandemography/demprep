
context("date_to_cohort")

## date_to_cohort_year --------------------------------------------------------

test_that("date_to_cohort_year gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   month_start = "Jan",
                                                   break_min = as.Date("2001-01-01"),
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2004"),
                            levels = as.character(2001:2004)))
    expect_identical(date_to_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   month_start = "Jan",
                                                   break_min = NULL,
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2004"),
                            levels = as.character(2001:2004)))
    expect_identical(date_to_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = as.character(2000:2004)))
    expect_identical(date_to_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = as.character(2000:2004)))
    expect_identical(date_to_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = FALSE,
                                                   break_min = NULL,
                                                   month_start = "Jul",
                                                   open_first = FALSE,
                                                   as_factor = TRUE),
                     factor(c("2003", "2001", "2005"),
                            levels = as.character(2001:2005)))
    expect_identical(date_to_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   break_min = as.Date("2000-07-01"),
                                                   month_start = "Jul",
                                                   open_first = TRUE,
                                                   as_factor = TRUE),
                     factor(c("2002", "2000", "2004"),
                            levels = c("<2000", as.character(2000:2004))))
    expect_identical(date_to_cohort_year(date = c("2003-03-20",
                                                            "2001-02-11",
                                                            "2004-12-30"),
                                                   label_year_start = TRUE,
                                                   month_start = "Jul",
                                                   break_min = as.Date("2005-07-01"),
                                                   open_first = TRUE,
                                                   as_factor = FALSE),
                     c("<2005", "<2005", "<2005"))
    expect_identical(date_to_cohort_year(date = c("2003-03-20",
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
                                         open_first = TRUE),
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
                                         open_first = TRUE),
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
                                         open_first = TRUE),
                     factor(c("1999", "2009", "2004"),
                            levels = c("<1999", 1999:2009)))
    expect_identical(date_to_cohort_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         break_min = NULL,
                                         month_start = "Apr",
                                         open_first = TRUE),
                     factor(c("1999", "2009", "2004"),
                            levels = c("<1999", 1999:2009)))
})

test_that("'date_to_cohort_year' throws correct error with invalid inputs", {
    expect_error(date_to_cohort_year(date = c("2003-03-20",
                                              "2001-02-11",
                                              "2004-12-30"),
                                     label_year_start = FALSE,
                                     month_start = "Jan",
                                     break_min = as.Date("2001-01-01"),
                                     open_first = TRUE,
                                     as_factor = TRUE),
                 "'open_first' is TRUE but 'label_year_start' is FALSE")
})



## date_to_cohort_multi -------------------------------------------------------

test_that("date_to_cohort_multi gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31")),
                     factor(c("2000-2005", "2010-2015", "2000-2005"),
                            levels = c("2000-2005", "2005-2010", "2010-2015")))
    expect_identical(date_to_cohort_multi(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31"),
                                          month_start = "Jul",
                                          open_first = TRUE),
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


## date_to_cohort_custom --------------------------------------------------

test_that("date_to_cohort_custom gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_custom(date = c("2000-01-01",
                                                    "2010-01-01",
                                                    "2004-12-31"),
                                           breaks = c("2000-01-01",
                                                      "2008-01-01",
                                                      "2015-01-01")),
                     factor(c("2000-2008", "2008-2015", "2000-2008"),
                            levels = c("<2000", "2000-2008", "2008-2015")))
    expect_identical(date_to_cohort_custom(date = c("2000-01-01",
                                                    "2010-01-01",
                                                    NA,
                                                    "2004-12-31"),
                                           breaks = c("2008-07-01",
                                                      "2015-07-01")),
                     factor(c("<2008", "2008-2015", NA, "<2008"),
                            levels = c("<2008", "2008-2015")))
    expect_identical(date_to_cohort_custom(date = character(),
                                           breaks = c("2000-03-01",
                                                      "2005-03-01"),
                                           open_first = FALSE),
                     factor(character(),
                            levels = "2000-2005"))
    expect_identical(date_to_cohort_custom(date = c("2003-03-20",
                                                    "2001-02-11",
                                                    "2010-12-30"),
                                           breaks = c("2000-01-01",
                                                      "2006-01-01",
                                                      "2020-01-01"),
                                           open_first = FALSE,
                                           as_factor = TRUE),
                     factor(c("2000-2006", "2000-2006", "2006-2020"),
                            levels = c("2000-2006", "2006-2020")))
    expect_identical(date_to_cohort_custom(date = c("2003-03-20",
                                                    "2001-02-11",
                                                    NA,
                                                    "2010-12-30",
                                                    "1999-03-02"),
                                           breaks = c("2000-03-01",
                                                      "2006-03-01",
                                                      "2020-03-01"),
                                           open_first = TRUE,
                                           as_factor = TRUE),
                     factor(c("2000-2006", "2000-2006", NA, "2006-2020", "<2000"),
                            levels = c("<2000", "2000-2006", "2006-2020")))
    expect_identical(date_to_cohort_custom(date = c(NA, NA),
                                           breaks = c("2001-04-01",
                                                      "2003-04-01")),
                     factor(c(NA, NA),
                            levels = c("<2001", "2001-2003")))
    expect_identical(date_to_cohort_custom(date = c(NA, NA),
                                           breaks = c("2001-04-01",
                                                      "2003-04-01"),
                                           open_first = FALSE),
                     factor(c(NA, NA),
                            levels = "2001-2003"))
    expect_identical(date_to_cohort_custom(date = character(),
                                            breaks = c("2001-04-01",
                                                       "2003-04-01")),
                     factor(character(), levels = c("<2001", "2001-2003")))
    expect_identical(date_to_cohort_custom(date = character(),
                                            breaks = c("2001-04-01",
                                                       "2003-04-01"),
                                            as_factor = FALSE),
                     character())
    expect_identical(date_to_cohort_custom(date = character(),
                                           breaks = character(),
                                           open_first = FALSE,
                                           as_factor = FALSE),
                     character())
})

test_that("date_to_cohort_custom throws expected error with invalid inputs", {
    expect_error(date_to_cohort_custom(date = "2000-01-01",
                                       breaks = character(),
                                       as_factor = FALSE),
                 "'breaks' has length 0")
})




## date_to_cohort_quarter -------------------------------------------------------

test_that("date_to_cohort_quarter gives correct answers with valid inputs", {
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
    expect_identical(date_to_cohort_quarter(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                            break_min = as.Date("2000-01-01"),
                                            open_first = FALSE,
                                            as_factor = TRUE),
                     factor(c("2003 Q1", "2001 Q1", "2010 Q4"),
                            levels = paste0(rep(2000:2010, each = 4),
                                            " Q",
                                            1:4)))
    expect_identical(date_to_cohort_quarter(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                            break_min = NULL,
                                            open_first = FALSE,
                                            as_factor = TRUE),
                     factor(c("2003 Q1", "2001 Q1", "2010 Q4"),
                            levels = paste0(rep(2001:2010, each = 4),
                                            " Q",
                                            1:4)))
    expect_identical(date_to_cohort_quarter(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                            break_min = NULL,
                                            open_first = FALSE,
                                            as_factor = FALSE),
                     c("2003 Q1", "2001 Q1", "2010 Q4"))
    expect_identical(date_to_cohort_quarter(date = c("2000-01-01", "2000-01-01"),
                                            break_min = NULL,
                                            open_first = FALSE,
                                            as_factor = TRUE),
                     factor(c("2000 Q1", "2000 Q1"),
                            levels = "2000 Q1"))
    expect_identical(date_to_cohort_quarter(date = c("2000-01-01", NA, "2000-01-01"),
                                            break_min = NULL,
                                            open_first = FALSE,
                                            as_factor = TRUE),
                     factor(c("2000 Q1", NA, "2000 Q1"),
                            levels = "2000 Q1"))
    expect_identical(date_to_cohort_quarter(date = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = character()))
    expect_identical(date_to_cohort_quarter(date = character(),
                                            as_factor = FALSE),
                     character())
    expect_identical(date_to_cohort_quarter(date = character(),
                                          break_min = "2001-07-01",
                                          open_first = TRUE,
                                          as_factor = TRUE),
                     factor(character(), levels = "<2001 Q3"))
    expect_identical(date_to_cohort_quarter(date = as.character(c(NA, NA)),
                                          break_min = "2001-07-01",
                                          open_first = TRUE,
                                          as_factor = TRUE),
                     factor(c(NA, NA), levels = "<2001 Q3"))
})


## date_to_cohort_month -------------------------------------------------------

test_that("date_to_cohort_month gives correct answers with valid inputs", {
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
    expect_identical(date_to_cohort_month(date = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = character()))
    expect_identical(date_to_cohort_month(date = character(),
                                          as_factor = FALSE),
                     character())
    expect_identical(date_to_cohort_month(date = character(),
                                          break_min = "2001-07-01",
                                          open_first = TRUE,
                                          as_factor = TRUE),
                     factor(character(), levels = "<2001 Jul"))
    expect_identical(date_to_cohort_month(date = as.character(c(NA, NA)),
                                          break_min = "2001-07-01",
                                          open_first = TRUE,
                                          as_factor = TRUE),
                     factor(c(NA, NA), levels = "<2001 Jul"))
    expect_identical(date_to_cohort_month(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                          break_min = as.Date("2000-01-01"),
                                          open_first = TRUE,
                                          as_factor = TRUE),
                     factor(c("2003 Mar", "2001 Feb", "2010 Dec"),
                            levels = c("<2000 Jan", paste(rep(2000:2010, each = 12), month.abb))))
    expect_identical(date_to_cohort_month(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                          break_min = NULL,
                                          open_first = FALSE,
                                          as_factor = TRUE),
                     factor(c("2003 Mar", "2001 Feb", "2010 Dec"),
                            levels = paste(rep(2001:2010, each = 12), month.abb)[-1]))
    expect_identical(date_to_cohort_month(date = c("2003-03-20", "2001-02-11", "2010-12-30"),
                                          break_min = NULL,
                                          open_first = FALSE,
                                          as_factor = FALSE),
                     c("2003 Mar", "2001 Feb", "2010 Dec"))
    expect_identical(date_to_cohort_month(date = c("2000-01-01", "2000-01-01"),
                                          break_min = NULL,
                                          open_first = FALSE,
                                          as_factor = TRUE),
                     factor(c("2000 Jan", "2000 Jan"),
                            levels = "2000 Jan"))
    expect_identical(date_to_cohort_month(date = c("2000-01-01", NA, "2000-01-01"),
                                          break_min = NULL,
                                          open_first = FALSE,
                                          as_factor = TRUE),
                     factor(c("2000 Jan", NA, "2000 Jan"),
                            levels = c("2000 Jan")))
})



