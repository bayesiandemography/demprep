
context("date_to_cohort")

## date_to_cohort_year --------------------------------------------------

test_that("date_to_cohort_year gives correct answers with valid inputs", {
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
                                         year_to = FALSE,
                                         open_left = TRUE),
                     factor(c("1999", "2009", "2004"),
                            levels = c("<1999", 1999:2009)))
})

