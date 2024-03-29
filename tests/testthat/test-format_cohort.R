
## format_cohort_year ---------------------------------------------------------

test_that("format_cohort_year works with valid input", {
    expect_identical(suppressMessages(format_cohort_year(x = c("2000", "2010", NA, "2004"))),
                     factor(c(2000, 2010, NA, 2004),
                            levels = c(as.character(2000:2010), NA),
                            exclude = NULL))
    expect_identical(suppressMessages(format_cohort_year(x = c("2000", "2010", NA, "2004"),
                                                         break_min = 2005)),
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
                 "'x' has invalid label \\[\"wrong\"\\]")
})


## format_cohort_multi --------------------------------------------------------

test_that("format_cohort_multi works with valid input", {
    ## all defaults
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000", "2010-2015", NA, "2004"))),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    ## has repeated values
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000", "2000", NA, "2004"))),
                     factor(c("2000-2005", "2000-2005", NA, "2000-2005"),
                            levels = c("2000-2005", NA),
                            exclude = NULL))
    ## 'month_start' is "Jul", but 'label_year_start' is TRUE (so labels unaffected)
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000", "2010-2015", NA, "2004"),
                                         month_start = "Jul",
                                         label_year_start = TRUE)),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    ## 'month_start' is "Jul" and 'label_year_start' is FALSE (so labels affected)
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000", "2010-2015", NA, "2004"),
                                         month_start = "Jul",
                                         label_year_start = FALSE)),
                     factor(c("1995-2000", "2010-2015", NA, "2000-2005"),
                            levels = c("1995-2000", "2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    ## width = 1; everything else default
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000-2001", "2010-2011", NA, "2004"),
                                         width = 1)),
                     factor(c("2000-2001", "2010-2011", NA, "2004-2005"),
                            levels = c(paste(2000:2010, 2001:2011, sep = "-"), NA_character_),
                            exclude = NULL))
    ## width = 1; label_year_start is FALSE, but month_start is "Jan"
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000", "2010", NA, "2004"),
                                         width = 1,
                                         month_start = "Jan",
                                         label_year_start = FALSE)),
                     factor(c("2000-2001", "2010-2011", NA, "2004-2005"),
                            levels = c(paste(2000:2010, 2001:2011, sep = "-"), NA_character_),
                            exclude = NULL))
    ## month_start is "Feb" and label_year_start is FALSE
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000", "2010", NA, "2004"),
                                         width = 1,
                                         month_start = "Feb",
                                         label_year_start = FALSE)),
                     factor(c("1999-2000", "2009-2010", NA, "2003-2004"),
                            levels = c(paste(1999:2009, 2000:2010, sep = "-"), NA_character_),
                            exclude = NULL))
    ## set 'break_min' but not 'open_first'
    expect_identical(suppressMessages(format_cohort_multi(x = c("2001", "2010-2015", NA, "2004-2005"),
                                         break_min = 1995)),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("<1995", "1995-2000", "2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    ## set 'open_first' but not 'break_min'
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2004-2005"),
                                         open_first = TRUE)),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("<2000", "2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    ## set 'open_first' and 'break_min'
    expect_identical(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2004-2005"),
                                         break_min = 1995,
                                         open_first = FALSE),
                     factor(c("2000-2005", "2010-2015", NA, "2000-2005"),
                            levels = c("1995-2000", "2000-2005", "2005-2010", "2010-2015", NA),
                            exclude = NULL))
    ## choice of 'origin' shifts intervals
    expect_identical(suppressMessages(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2004-2005"),
                                         origin = 2004,
                                         width = 20)),
                     factor(c("1984-2004", "2004-2024", NA, "2004-2024"),
                            levels = c("1984-2004", "2004-2024", NA),
                            exclude = NULL))
    ## x has length 0
    expect_identical(format_cohort_multi(x = character()),
                     factor())
    ## x has no non-NA values
    expect_identical(format_cohort_multi(x = c(NA, NA)),
                     factor(c(NA, NA),
                            levels = NA_character_,
                            exclude = NULL))
})

test_that("format_cohort_multi gives correct error with invalid inputs", {
    expect_error(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "wrong")),
                 "'x' has invalid label \\[\"wrong\"\\]")
    expect_error(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2015+"),
                                     open_first = FALSE),
                 "'x' has interval \\[\"2015\\+\"\\] that is open on the right")
    expect_error(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "<2000"),
                                     open_first = FALSE),
                 "'open_first' is FALSE but 'x' has open interval \\[\"<2000\"\\]")
    expect_error(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2000"),
                                     break_min = 2005,
                                     open_first = FALSE),
                 "'open_first' is FALSE but 'x' has interval \\[\"2000-2001\"\\] that starts below 'break_min' \\[2005\\]")    
    expect_error(format_cohort_multi(x = c("2000-2001", "2010-2015", NA, "2015-2025"),
                                     break_min = 2005,
                                     open_first = TRUE),
                 "'x' has interval \\[\"2015-2025\"\\] that intersects two or more intervals formed using 'origin = 2000' and 'width = 5'")
})


## format_cohort_custom -------------------------------------------------------

test_that("format_cohort_custom works with valid input", {
    ## includes open intervals
    expect_identical(suppressMessages(format_cohort_custom(x = c("<2000", "2000", "2010-2015", NA, "2004"),
                                          breaks = c(2000, 2003, 2006, 2020))),
                     factor(c("<2000", "2000-2003", "2006-2020", NA, "2003-2006"),
                            levels = c("<2000", "2000-2003", "2003-2006", "2006-2020", NA),
                            exclude = NULL))
    ## no open intervals
    expect_identical(suppressMessages(format_cohort_custom(x = c("2000", "2001", "2010-2015", NA, "2004-2005"),
                                          breaks = c(2000, 2003, 2006, 2020))),
                     factor(c("2000-2003", "2000-2003", "2006-2020", NA, "2003-2006"),
                            levels = c("2000-2003", "2003-2006", "2006-2020", NA),
                            exclude = NULL))
    ## 'open_first' is TRUE
    expect_identical(format_cohort_custom(x = c("2000-2001", "2010-2015", NA, "2004-2005", "1990-1991"),
                                          breaks = c(2000, 2003, 2006, 2020),
                                          open_first = TRUE),
                     factor(c("2000-2003", "2006-2020", NA, "2003-2006", "<2000"),
                            levels = c("<2000", "2000-2003", "2003-2006", "2006-2020", NA),
                            exclude = NULL))
    ## has repeated values
    expect_identical(format_cohort_custom(x = c("2000-2001", "2000-2001", NA, "2004-2005", "1990-1991"),
                                          breaks = c(2000, 2003, 2006, 2020),
                                          open_first = TRUE),
                     factor(c("2000-2003", "2000-2003", NA, "2003-2006", "<2000"),
                            levels = c("<2000", "2000-2003", "2003-2006", "2006-2020", NA),
                            exclude = NULL))
    ## 'x' is length 0
    expect_identical(format_cohort_custom(x = character(),
                                          breaks = integer(),
                                          open_first = FALSE),
                     factor())
})

test_that("format_cohort_custom gives correct error with invalid inputs", {
    expect_error(format_cohort_custom(x = "2000-2010",
                                      breaks = integer()),
                 "'breaks' has length 0")
    expect_error(format_cohort_custom(x = c("2000-2001", "2010-2005", NA, "wrong"),
                                      breaks = c(2000, 2020)),
                 "'x' has invalid label \\[\"wrong\"\\]")
    expect_error(format_cohort_custom(x = "2000+",
                                      breaks = c(1990, 2000)),
                 "'x' has interval \\[\"2000\\+\"\\] that is open on the right")
    expect_error(suppressMessages(format_cohort_custom(x = "<1995",
                                                       breaks = c(1990, 2000))),
                 "'x' has open interval \\[\"<1995\"\\] that ends above minimum for 'breaks' \\[1990\\]")
    expect_error(format_cohort_custom(x = "<1990",
                                      breaks = c(1990, 2000),
                                      open_first = FALSE),
                 "'open_first' is FALSE but 'x' has open interval \\[\"<1990\"\\]")
    expect_error(format_cohort_custom(x = "1980-1985",
                                      breaks = c(1990, 2000),
                                      open_first = FALSE),
                 "'open_first' is FALSE but 'x' has interval \\[\"1980-1985\"\\] that starts below lowest value of 'breaks' \\[1990\\]")
    expect_error(suppressMessages(format_cohort_custom(x = "2000-2005",
                                                       breaks = c(1990, 2000))),
                 "'x' has interval \\[\"2000-2005\"\\] that ends above highest value of 'breaks' \\[2000\\]")
    expect_error(suppressMessages(format_cohort_custom(x = "1994-1996",
                                                       breaks = c(1990, 1995, 2000))),
                 "'x' has interval \\[\"1994-1996\"\\] that intersects two or more intervals formed using 'breaks'")
})


## format_cohort_quarter ------------------------------------------------------

test_that("format_cohort_quarter works with valid input", {
    expect_identical(suppressMessages(format_cohort_quarter(x = c("2000 Q4", "2010 Q1", NA, "2004 Q2"))),
                     factor(c("2000 Q4", "2010 Q1", NA, "2004 Q2"),
                            levels = c("2000 Q4",
                                       paste(rep(2001:2009, each = 4),
                                             paste0("Q", 1:4)),
                                       "2010 Q1",
                                       NA),
                            exclude = NULL))
    expect_identical(suppressMessages(format_cohort_quarter(x = c("2000 Q4", "2010 Q1", NA, "<2004 Q2"))),
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
                 "'x' has invalid label \\[\"wrong\"\\]")
    expect_error(format_cohort_quarter(x = c("2000 Q5", "2010 Q3", NA)),
                 "'x' has invalid label \\[\"2000 Q5\"\\]")
})


## format_cohort_month --------------------------------------------------------

test_that("format_cohort_month works with valid input", {
    expect_identical(suppressMessages(format_cohort_month(x = c("2000 Jan", "2001 Feb", NA, "2001 Mar"))),
                     factor(c("2000 Jan", "2001 Feb", NA, "2001 Mar"),
                            levels = c(paste("2000", month.abb),
                                       "2001 Jan", "2001 Feb", "2001 Mar",
                                       NA),
                            exclude = NULL))
    expect_identical(suppressMessages(format_cohort_month(x = c("2000 Jan", "<2001 Feb", NA, "2001 Mar"))),
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
                 "'x' has invalid label \\[\"wrong\"\\]")
    expect_error(format_cohort_month(x = c("2000 JAN", "2010 Q3", NA)),
                 "'x' has invalid label \\[\"2000 JAN\"\\]")
})
