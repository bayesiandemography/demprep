
context("format_cohort")

## format_cohort_year ---------------------------------------------------------

test_that("format_cohort_year works with valid input", {
    expect_identical(format_cohort_year(x = c("2000", "2010", NA, "2004")),
                     factor(c(2000, 2010, NA, 2004),
                            levels = c(as.character(2000:2010), NA),
                            exclude = NULL))
    expect_identical(format_cohort_year(x = c("2000", "2010", NA, "2004"),
                                        break_min = 2005),
                     factor(c("<2005", 2010, NA, "<2005"),
                            levels = c("<2005", 2005:2010, NA),
                            exclude = NULL))
    expect_identical(format_cohort_year(x = character()),
                     factor())
    expect_identical(format_cohort_year(x = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = NA_character_,
                            exclude = NULL))
})

test_that("format_cohort_year gives correct error with invalid inputs", {
    expect_error(format_cohort_year(x = c("2000", "2010", NA, "wrong")),
                 "\"wrong\" is not a valid label")
})


## format_cohort_multi --------------------------------------------------------

test_that("format_cohort_multi works with valid input", {
    expect_identical(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2004-2005")),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    expect_identical(format_cohort_multi(x = c("2000-2001", "2010-2011", NA, "2004-2005"),
                                         width = 1),
                     factor(c("2000", "2010", NA, "2004"),
                            levels = c(2000:2010, NA_character_),
                            exclude = NULL))
    expect_identical(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2004-2005"),
                                         break_min = 1995),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("<1995", "1995-2000", "2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    expect_identical(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2004-2005"),
                                         open_first = TRUE),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("<2000", "2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    expect_identical(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2004-2005"),
                                         break_min = 1995,
                                         open_first = FALSE),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("1995-2000", "2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    expect_identical(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2004-2005"),
                                         origin = 2004,
                                         width = 20),
                     factor(c("1984-2004", "2004-2024", NA, "2004-2024"),
                            levels = c("1984-2004", "2004-2024", NA),
                            exclude = NULL))
    expect_identical(format_cohort_multi(x = character()),
                     factor())
    expect_identical(format_cohort_multi(x = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = NA_character_,
                            exclude = NULL))
})

test_that("format_cohort_multi gives correct error with invalid inputs", {
    expect_error(format_cohort_multi(x = c("2000-2001", "2010-2005", NA, "wrong")),
                 "\"wrong\" is not a valid label")
})


## format_cohort_custom -------------------------------------------------------

test_that("format_cohort_custom works with valid input", {
    expect_identical(format_cohort_custom(x = c("<2000", "2000-2001", "2010-2015", NA, "2004"),
                                          breaks = c(2000, 2003, 2006, 2020)),
                     factor(c("<2000", "2000-2003", "2006-2020", NA, "2003-2006"),
                            levels = c("<2000", "2000-2003", "2003-2006", "2006-2020", NA),
                            exclude = NULL))
    expect_identical(format_cohort_custom(x = c("2000", "2000-2001", "2010-2015", NA, "2004"),
                                          breaks = c(2000, 2003, 2006, 2020)),
                     factor(c("2000-2003", "2000-2003", "2006-2020", NA, "2003-2006"),
                            levels = c("2000-2003", "2003-2006", "2006-2020", NA),
                            exclude = NULL))
    expect_identical(format_cohort_custom(x = c("2000-2001", "2010-2015", NA, "2004", "1990"),
                                          breaks = c(2000, 2003, 2006, 2020),
                                          open_first = TRUE),
                     factor(c("2000-2003", "2006-2020", NA, "2003-2006", "<2000"),
                            levels = c("<2000", "2000-2003", "2003-2006", "2006-2020", NA),
                            exclude = NULL))
    expect_identical(format_cohort_custom(x = character(),
                                          breaks = integer(),
                                          open_first = FALSE),
                     factor())
})

test_that("format_cohort_custom gives correct error with invalid inputs", {
    expect_error(format_cohort_custom(x = "2000-2010", breaks = integer()),
                 "'breaks' has length 0")
    expect_error(format_cohort_custom(x = c("2000-2001", "2010-2005", NA, "wrong"),
                                      breaks = c(2000, 2020)),
                 "\"wrong\" is not a valid label")
})


## format_cohort_quarter ------------------------------------------------------

test_that("format_cohort_quarter works with valid input", {
    expect_identical(format_cohort_quarter(x = c("2000 Q4", "2010 Q1", NA, "2004 Q2")),
                     factor(c("2000 Q4", "2010 Q1", NA, "2004 Q2"),
                            levels = c("2000 Q4",
                                       paste(rep(2001:2009, each = 4),
                                             paste0("Q", 1:4)),
                                       "2010 Q1",
                                       NA),
                            exclude = NULL))
    expect_identical(format_cohort_quarter(x = c("2000 Q4", "2010 Q1", NA, "<2004 Q2")),
                     factor(c("<2004 Q2", "2010 Q1", NA, "<2004 Q2"),
                            levels = c("<2004 Q2", "2004 Q2", "2004 Q3", "2004 Q4",
                                       paste(rep(2005:2009, each = 4),
                                             paste0("Q", 1:4)),
                                       "2010 Q1",
                                       NA),
                            exclude = NULL))
    expect_identical(format_cohort_quarter(x = character()),
                     factor())
    expect_identical(format_cohort_quarter(x = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = NA_character_,
                            exclude = NULL))
})

test_that("format_cohort_quarter gives correct error with invalid inputs", {
    expect_error(format_cohort_quarter(x = c("2000 Q4", "2010 Q3", NA, "wrong")),
                 "\"wrong\" is not a valid label")
    expect_error(format_cohort_quarter(x = c("2000 Q5", "2010 Q3", NA)),
                 "\"2000 Q5\" is not a valid label")
})


## format_cohort_month --------------------------------------------------------

test_that("format_cohort_month works with valid input", {
    expect_identical(format_cohort_month(x = c("2000 Jan", "2001 Feb", NA, "2001 Mar")),
                     factor(c("2000 Jan", "2001 Feb", NA, "2001 Mar"),
                            levels = c(paste("2000", month.abb),
                                       "2001 Jan", "2001 Feb", "2001 Mar",
                                       NA),
                            exclude = NULL))
    expect_identical(format_cohort_month(x = c("2000 Jan", "<2001 Feb", NA, "2001 Mar")),
                     factor(c("<2001 Feb", "<2001 Feb", NA, "2001 Mar"),
                            levels = c("<2001 Feb", "2001 Feb", "2001 Mar",
                                       NA),
                            exclude = NULL))
    expect_identical(format_cohort_month(x = character()),
                     factor())
    expect_identical(format_cohort_month(x = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = NA_character_,
                            exclude = NULL))
})

test_that("format_cohort_month gives correct error with invalid inputs", {
    expect_error(format_cohort_month(x = c("2000 Jan", "2010 Feb", NA, "wrong")),
                 "\"wrong\" is not a valid label")
    expect_error(format_cohort_month(x = c("2000 JAN", "2010 Q3", NA)),
                 "\"2000 JAN\" is not a valid label")
})
