
context("helper-format")

## format_period_month_quarter_year -------------------------------------------

test_that("'format_period_month_quarter_year' gives correct answer with valid inputs - years", {
    x <- c("2000", "2005", "1990", NA)
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_period_year)
    ans_expected <- factor(x,
                           levels = c(1990:2005, NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_period_year)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    x <- as.character(c(NA, NA, NA))
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_period_year)
    ans_expected <- factor(x,
                           levels = NA,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format_period_month_quarter_year' throws correct error with invalid inputs - years", {
    expect_error(format_period_month_quarter_year(x = c("2000", "2001+"),
                                                  parse_fun = parse_integers,
                                                  labels_fun = make_labels_period_year),
                 "'x' has open interval \\[\"2001\\+\"\\]")
})

test_that("'format_period_month_quarter_year' gives correct answer with valid inputs - quarters", {
    x <- c("2000 Q1", "2005 Q2", "1990 Q4", NA)
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                     parse_fun = parse_quarters,
                                                     labels_fun = make_labels_period_quarter)
    date <- seq.Date(from = as.Date("1990-10-01"),
                     to = as.Date("2005-04-01"),
                     by = "quarter")
    levels <- c(paste(format(date, "%Y"), quarters(date)),
                NA)
    ans_expected <- factor(x,
                           levels = levels,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                     parse_fun = parse_quarters,
                                                     labels_fun = make_labels_period_quarter)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    x <- as.character(c(NA, NA, NA))
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                     parse_fun = parse_quarters,
                                                     labels_fun = make_labels_period_quarter)
    ans_expected <- factor(x,
                           levels = NA,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format_period_month_quarter_year' throws correct error with invalid inputs - quarters", {
    expect_error(format_period_month_quarter_year(x = c("2000 Q2", "2001 Q3+"),
                                                  parse_fun = parse_quarters,
                                                  labels_fun = make_labels_period_quarter),
                 "'x' has invalid label \\[\"2001 Q3\\+\"\\]")
    expect_error(format_period_month_quarter_year(x = c("2000 Q2", "<2001 Q3"),
                                                  parse_fun = parse_quarters,
                                                  labels_fun = make_labels_period_quarter),
                 "'x' has open interval \\[\"<2001 Q3\"\\]")
})

test_that("'format_period_month_quarter_year' gives correct answer with valid inputs - months", {
    x <- c("2000 Jan", "2005 Mar", "1990 Dec", NA)
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                   parse_fun = parse_months,
                                                   labels_fun = make_labels_period_month)
    date <- seq.Date(from = as.Date("1990-12-01"),
                     to = as.Date("2005-03-01"),
                     by = "month")
    levels <- c(format(date, "%Y %b"), NA)
    ans_expected <- factor(x,
                           levels = levels,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                   parse_fun = parse_months,
                                                   labels_fun = make_labels_period_month)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    x <- as.character(c(NA, NA, NA))
    ans_obtained <- format_period_month_quarter_year(x = x,
                                                   parse_fun = parse_months,
                                                   labels_fun = make_labels_period_month)
    ans_expected <- factor(x,
                           levels = NA,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format_period_month_quarter_year' throws correct error with invalid inputs - months", {
    expect_error(format_period_month_quarter_year(x = c("2000 Mar", "2001 Jun+"),
                                                  parse_fun = parse_months,
                                                  labels_fun = make_labels_period_month),
                 "'x' has invalid label \\[\"2001 Jun\\+\"\\]")
    expect_error(format_period_month_quarter_year(x = c("2000 Mar", "<2001 Jun"),
                                                  parse_fun = parse_months,
                                                  labels_fun = make_labels_period_month),
                 "'x' has open interval \\[\"<2001 Jun\"\\]")
})
  

## make_i_interval ------------------------------------------------------------

test_that("'make_i_interval' gives correct answer with valid inputs", {
    expect_identical(make_i_interval(low = c(1990L, 2002L, 2000L, 2005L, 2006L, 2020L, NA),
                                     up = c(1995L, 2005L, 2001L, 2020L, 2019L, NA, NA),
                                     breaks = c(2000L, 2005L, 2020L),
                                     open_first = TRUE,
                                     open_last = TRUE),
                     c(1L, 2L, 2L, 3L, 3L, 4L, NA))
    expect_identical(make_i_interval(low = c(2002L, 2000L, 2005L, 2006L, 2020L, NA),
                                     up = c(2005L, 2001L, 2020L, 2019L, NA, NA),
                                     breaks = c(2000L, 2005L, 2020L),
                                     open_first = FALSE,
                                     open_last = TRUE),
                     c(1L, 1L, 2L, 2L, 3L, NA))
    expect_identical(make_i_interval(low = c(2002L, 2000L, 2005L, 2006L, 2019L, NA),
                                     up = c(2010L, 2001L, 2020L, 2019L, NA, NA),
                                     breaks = c(2000L, 2005L, 2020L),
                                     open_first = FALSE,
                                     open_last = TRUE),
                     c(-1L, 1L, 2L, 2L, -1L, NA))
    expect_identical(make_i_interval(low = NA_integer_, 
                                     up = 2010L,
                                     breaks = c(2000L, 2005L, 2020L),
                                     open_first = TRUE,
                                     open_last = TRUE),
                     -1L)
})

