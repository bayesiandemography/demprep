
## date_to_cohort_period_year -------------------------------------------------

test_that("date_to_cohort_period_year gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                                month_start = "Jan",
                                                label_year_start = TRUE),
                     c(2000L, 2010L, 2004L))
    expect_identical(date_to_cohort_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         month_start = "Jun",
                                         label_year_start = TRUE),
                     c(1999L, 2009L, 2004L))
    expect_identical(date_to_cohort_period_year(date = c("2000-01-01",
                                                  "2010-01-01",
                                                  "2004-12-31"),
                                         label_year_start = FALSE,
                                         month_start = "Apr"),
                     c(2000L, 2010L, 2005L))
    expect_identical(date_to_cohort_period_year(date = c("2003-03-20",
                                                  "2001-02-11",
                                                  "2004-12-30"),
                                         label_year_start = TRUE,
                                         month_start = "Jan"),
                     c(2003L, 2001L, 2004L))
    expect_identical(date_to_cohort_period_year(date = c("2003-03-20",
                                                  "2001-02-11",
                                                  "2004-12-30"),
                                         label_year_start = TRUE,
                                         month_start = "Jul"),
                     c(2002L, 2000L, 2004L))
    expect_identical(date_to_cohort_period_year(date = c("2003-03-20",
                                                  "2001-02-11",
                                                  NA,
                                                  "2004-12-30"),
                                         label_year_start = TRUE,
                                         month_start = "Jul"),
                     c(2002L, 2000L, NA, 2004L))
    expect_identical(date_to_cohort_period_year(date = c("2003-03-20",
                                                  "2001-02-11",
                                                  "2004-12-30"),
                                         label_year_start = FALSE,
                                         month_start = "Jul"),
                     c(2003L, 2001L, 2005L))
    expect_identical(date_to_cohort_period_year(date = c(NA, NA),
                                                label_year_start = TRUE,
                                                month_start = "Jan"),
                     c(NA_integer_, NA_integer_))
    expect_identical(date_to_cohort_period_year(date = character(),
                                                label_year_start = TRUE,
                                                month_start = "Jan"),
                     integer())
})


## date_to_cohort_period_quarter ---------------------------------------------------

test_that("date_to_cohort_period_quarter gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_period_quarter(date = c("2000-01-01",
                                                     "2010-01-01",
                                                     "2004-12-31")),
                     c("2000 Q1", "2010 Q1", "2004 Q4"))
    expect_identical(date_to_cohort_period_quarter(date = c(NA, "2004-12-31")),
                     c(NA, "2004 Q4"))
    expect_identical(date_to_cohort_period_quarter(date = c("2003-03-20", "2001-02-11", "2010-12-30")),
                     c("2003 Q1", "2001 Q1", "2010 Q4"))
    expect_identical(date_to_cohort_period_quarter(date = c("2000-01-01", "2000-01-01")),
                     c("2000 Q1", "2000 Q1"))
    expect_identical(date_to_cohort_period_quarter(date = c("2000-01-01",
                                                     "2000-05-11",
                                                     NA,
                                                     "2001-02-28")),
                     c("2000 Q1", "2000 Q2", NA,  "2001 Q1"))
    expect_identical(date_to_cohort_period_quarter(date = "2000-01-01"),
                     "2000 Q1")
    expect_identical(date_to_cohort_period_quarter(date = c(NA, NA)),
                     c(NA_character_, NA_character_))
})
    

## date_to_cohort_period_month ---------------------------------------------------

test_that("date_to_cohort_period_month gives correct answers with valid inputs", {
    expect_identical(date_to_cohort_period_month(date = c("2000-01-01",
                                                   "2000-05-11",
                                                   NA,
                                                   "2001-02-28")),
                     c("2000 Jan", "2000 May", NA,  "2001 Feb"))
    expect_identical(date_to_cohort_period_month(date = "2000-01-01"),
                     "2000 Jan")
    expect_identical(date_to_cohort_period_month(date = c("2000-01-01",
                                                   "2010-01-01",
                                                   "2004-12-31")),
                     c("2000 Jan", "2010 Jan", "2004 Dec"))
    expect_identical(date_to_cohort_period_month(date = c(NA, "2004-12-31")),
                     c(NA, "2004 Dec"))
    expect_identical(date_to_cohort_period_month(date = c(NA, NA)),
                     c(NA_character_, NA_character_))
    expect_identical(date_to_cohort_period_month(date = c("2003-03-20", "2001-02-11", "2010-12-30")),
                     c("2003 Mar", "2001 Feb", "2010 Dec"))
    expect_identical(date_to_cohort_period_month(date = c("2000-01-01", "2000-01-01")),
                     c("2000 Jan", "2000 Jan"))
    expect_identical(date_to_cohort_period_month(date = c("2000-01-01", NA, "2000-01-01")),
                     c("2000 Jan", NA, "2000 Jan"))
})
