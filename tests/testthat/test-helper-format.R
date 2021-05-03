
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
    ans_expected <- factor(x,
                           levels = c(seq.Date(from = as.Date("1990-10-01"),
                                               to = as.Date("2005-07-01"),
                                               by = "quarter"),
                                      NA),
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
                 "'x' has open interval \\[\"2001 Q3\\+\"\\]")
})

test_that("'format_period_month_month_year' gives correct answer with valid inputs - months", {
    x <- c("2000 Jan", "2005 Mar", "1990 Dec", NA)
    ans_obtained <- format_period_month_month_year(x = x,
                                                   parse_fun = parse_months,
                                                   labels_fun = make_labels_period_month)
    ans_expected <- factor(x,
                           levels = c(seq.Date(from = as.Date("1990-12-01"),
                                               to = as.Date("2005-04-01"),
                                               by = "month"),
                                      NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- format_period_month_month_year(x = x,
                                                   parse_fun = parse_months,
                                                   labels_fun = make_labels_period_month)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    x <- as.character(c(NA, NA, NA))
    ans_obtained <- format_period_month_month_year(x = x,
                                                   parse_fun = parse_months,
                                                   labels_fun = make_labels_period_month)
    ans_expected <- factor(x,
                           levels = NA,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format_period_month_month_year' throws correct error with invalid inputs - months", {
    expect_error(format_period_month_quarter_year(x = c("2000 Mar", "2001 Jun+"),
                                                  parse_fun = parse_months,
                                                  labels_fun = make_labels_period_month),
                 "'x' has open interval \\[\"2001 Jun\\+\"\\]")
})
  

