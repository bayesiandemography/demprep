
context("helper-format")

## date_to_month --------------------------------------------------------------

test_that("'date_to_month_label' gives correct answer with valid inputs", {
    expect_identical(date_to_month_label(as.Date("2000-12-13")),
                     "2000 Dec")
    expect_identical(date_to_month_label(as.Date("2003-01-02")),
                     "2003 Jan")
})

## date_to_quarter ------------------------------------------------------------

test_that("'date_to_quarter_label' gives correct answer with valid inputs", {
    expect_identical(date_to_quarter_label(as.Date("2000-12-13")),
                     "2000 Q4")
    expect_identical(date_to_quarter_label(as.Date("2003-01-02")),
                     "2003 Q1")
})


## format_age_month_quarter_year ----------------------------------------------

test_that("'format_age_month_quarter_year' gives correct answer with valid inputs", {
    ## 'x' is plain integers
    x <- c(4, 11, NA)
    ans_obtained <- format_age_month_quarter_year(x = x,
                                                  break_min = 0,
                                                  break_max = 100,
                                                  open_last = TRUE)
    ans_expected <- factor(x,
                           levels = c(0:99, "100+", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## 'x' is has repeated value
    x <- c(4, 11, NA, 11)
    ans_obtained <- format_age_month_quarter_year(x = x,
                                                  break_min = 0,
                                                  break_max = 100,
                                                  open_last = TRUE)
    ans_expected <- factor(x,
                           levels = c(0:99, "100+", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## 'x' includes open intervals
    x <- c(4, 11, NA, "100+", "110+")
    ans_obtained <- format_age_month_quarter_year(x = x,
                                                  break_min = 0,
                                                  break_max = 100,
                                                  open_last = TRUE)
    ans_expected <- factor(c(4, 11, NA, "100+", "100+"),
                           levels = c(0:99, "100+", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## 'break_min', 'break_max' both NULL
    x <- c(4, 11, NA, "100+", "110+")
    ans_obtained <- format_age_month_quarter_year(x = x,
                                                  break_min = NULL,
                                                  break_max = NULL,
                                                  open_last = TRUE)
    ans_expected <- factor(c(4, 11, NA, "100+", "100+"),
                           levels = c(4:99, "100+", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## non-default 'open_first', 'break_min' and 'break_max'
    x <- c(14, 11, NA, 10)
    ans_obtained <- format_age_month_quarter_year(x = x,
                                                  break_min = 10,
                                                  break_max = 20,
                                                  open_last = FALSE)
    ans_expected <- factor(x,
                           levels = c(10:19, NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## 'x' has length 0, break_min and break_max both supplied
    ans_obtained <- format_age_month_quarter_year(x = integer(),
                                                  break_min = 0,
                                                  break_max = 100,
                                                  open_last = TRUE)
    ans_expected <- factor(integer(),
                           levels = c(0:99, "100+"))
    expect_identical(ans_obtained, ans_expected)
    ## 'x' has length 0, break_min is NULL
    ans_obtained <- format_age_month_quarter_year(x = integer(),
                                                  break_min = NULL,
                                                  break_max = 100,
                                                  open_last = TRUE)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    ## 'x' all NA, break_min and break_max both supplied
    ans_obtained <- format_age_month_quarter_year(x = NA,
                                                  break_min = 0,
                                                  break_max = 100,
                                                  open_last = TRUE)
    ans_expected <- factor(NA,
                           levels = c(0:99, "100+", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## 'x' all NA, break_min is NULL
    ans_obtained <- format_age_month_quarter_year(x = NA,
                                                  break_min = NULL,
                                                  break_max = 100,
                                                  open_last = TRUE)
    ans_expected <- factor(NA,
                           levels = NA,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})
        
test_that("'format_age_month_quarter_year' throws correct error with invalid inputs - years", {
    expect_error(format_age_month_quarter_year(x = c("10", "<5"),
                                               break_min = NULL,
                                               break_max = NULL,
                                               open_last = TRUE),
                 "'x' has interval \\[\"<5\"\\] that is open on the left")
    expect_error(format_age_month_quarter_year(x = c("10", "0"),
                                               break_min = 5,
                                               break_max = NULL,
                                               open_last = TRUE),
                 "'x' has interval \\[\"0\"\\] that starts below 'break_min' \\[5\\]")
    expect_error(format_age_month_quarter_year(x = c("5+", "0"),
                                               break_min = 0,
                                               break_max = 10,
                                               open_last = TRUE),
                 "'x' has open interval \\[\"5\\+\"\\] that starts below 'break_max' \\[10\\]")
    expect_error(format_age_month_quarter_year(x = c("5+", "0"),
                                               break_min = 0,
                                               break_max = 10,
                                               open_last = FALSE),
                 "'open_last' is FALSE but 'x' has open interval \\[\"5\\+\"\\]")
    expect_error(format_age_month_quarter_year(x = c("10", "0"),
                                               break_min = 0,
                                               break_max = 5,
                                               open_last = FALSE),
                 "'x' has interval \\[\"10\"\\] that ends above 'break_max' \\[5\\]")
})



## format_cohort_month_quarter_year -------------------------------------------

test_that("'format_cohort_month_quarter_year' gives correct answer with valid inputs - years", {
    x <- c("2000", "2005", "1990", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                     break_min_lab_fun = I,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_cohort_year)
    ans_expected <- factor(x,
                           levels = c(1990:2005, NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## repeated values
    x <- c("2000", "2000", NA, "1990", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                     break_min_lab_fun = I,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_cohort_year)
    ans_expected <- factor(x,
                           levels = c(1990:2000, NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2000", "2005", "1990", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = 1980,
                                                     open_first = NULL,
                                                     break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                     break_min_lab_fun = I,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_cohort_year)
    ans_expected <- factor(x,
                           levels = c("<1980", 1980:2005, NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2000", "2005", "1990", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = 2002,
                                                     open_first = NULL,
                                                     break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                     break_min_lab_fun = I,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_cohort_year)
    ans_expected <- factor(c("<2002", "2005", "<2002", NA),
                           levels = c("<2002", 2002:2005, NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                     break_min_lab_fun = I,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_cohort_year)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    x <- as.character(c(NA, NA, NA))
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                     break_min_lab_fun = I,
                                                     parse_fun = parse_integers,
                                                     labels_fun = make_labels_cohort_year)
    ans_expected <- factor(x,
                           levels = NA,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format_cohort_month_quarter_year' throws correct error with invalid inputs - years", {
    expect_error(format_cohort_month_quarter_year(x = c("2000", "2001+"),
                                                  break_min = NULL,
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                  break_min_lab_fun = I,
                                                  parse_fun = parse_integers,
                                                  labels_fun = make_labels_cohort_year),
                 "'x' has interval \\[\"2001\\+\"\\] that is open on the right")
    expect_error(format_cohort_month_quarter_year(x = c("2000", "<2001"),
                                                  break_min = 2000,
                                                  open_first = NULL,
                                                  break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                  break_min_lab_fun = I,
                                                  parse_fun = parse_integers,
                                                  labels_fun = make_labels_cohort_year),
                 "'x' has open interval \\[\"<2001\"\\] that ends above 'break_min' \\[2000\\]")
    expect_error(format_cohort_month_quarter_year(x = c("2000", "<2001"),
                                                  break_min = 2000,
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                  break_min_lab_fun = I,
                                                  parse_fun = parse_integers,
                                                  labels_fun = make_labels_cohort_year),
                 "'open_first' is FALSE but 'x' has open interval \\[\"<2001\"\\]")
    expect_error(format_cohort_month_quarter_year(x = c("2000", "1999"),
                                                  break_min = 2000,
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_non_negative_integer_scalar,
                                                  break_min_lab_fun = I, 
                                                  parse_fun = parse_integers,
                                                  labels_fun = make_labels_cohort_year),
                 "'open_first' is FALSE but 'x' has interval \\[\"1999\"\\] that starts below 'break_min' \\[2000\\]")
})


test_that("'format_cohort_month_quarter_year' gives correct answer with valid inputs - quarters", {
    x <- c("2000 Q1", "2005 Q4", "1990 Q1", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                     break_min_lab_fun = date_to_quarter_label,
                                                     parse_fun = parse_quarters,
                                                     labels_fun = make_labels_cohort_quarter)
    ans_expected <- factor(x,
                           levels = c(paste(rep(1990:2005, each = 4),
                                            paste0("Q", 1:4)),
                                      NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2000 Q1", "2005 Q4", "1990 Q1", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = "1980 Q1",
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                     break_min_lab_fun = date_to_quarter_label,
                                                     parse_fun = parse_quarters,
                                                     labels_fun = make_labels_cohort_quarter)
    ans_expected <- factor(x,
                           levels = c(paste(rep(1980:2005, each = 4),
                                            paste0("Q", 1:4)),
                                      NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2000 Q1", "2005 Q4", "1990 Q1", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = "2002 Q3",
                                                     open_first = NULL,
                                                     break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                     break_min_lab_fun = date_to_quarter_label,
                                                     parse_fun = parse_quarters,
                                                     labels_fun = make_labels_cohort_quarter)
    ans_expected <- factor(c("<2002 Q3", "2005 Q4", "<2002 Q3", NA),
                           levels = c("<2002 Q3", "2002 Q3", "2002 Q4",
                                      paste(rep(2003:2005, each = 4),
                                            paste0("Q", 1:4)),
                                      NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                     break_min_lab_fun = date_to_quarter_label,
                                                     parse_fun = parse_quarters,
                                                     labels_fun = make_labels_cohort_quarter)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    x <- as.character(c(NA, NA, NA))
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                     break_min_lab_fun = date_to_quarter_label,
                                                     parse_fun = parse_quarters,
                                                     labels_fun = make_labels_cohort_quarter)
    ans_expected <- factor(x,
                           levels = NA,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format_cohort_month_quarter_year' throws correct error with invalid inputs - quarters", {
    expect_error(format_cohort_month_quarter_year(x = c("2000 Q1", "2001 Q2+"),
                                                  break_min = NULL,
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                  break_min_lab_fun = date_to_quarter_label,
                                                  parse_fun = parse_quarters,
                                                  labels_fun = make_labels_cohort_quarter),
                 "'x' has interval \\[\"2001 Q2\\+\"\\] that is open on the right")
    expect_error(format_cohort_month_quarter_year(x = c("2000 Q3", "<2001 Q3"),
                                                  break_min = "2000 Q3",
                                                  open_first = NULL,
                                                  break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                  break_min_lab_fun = date_to_quarter_label,
                                                  parse_fun = parse_quarters,
                                                  labels_fun = make_labels_cohort_quarter),
                 "'x' has open interval \\[\"<2001 Q3\"\\] that ends above 'break_min' \\[\"2000 Q3\"\\]")
    expect_error(format_cohort_month_quarter_year(x = c("2000 Q1", "<2001 Q1"),
                                                  break_min = NULL,
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                  break_min_lab_fun = date_to_quarter_label,
                                                  parse_fun = parse_quarters,
                                                  labels_fun = make_labels_cohort_quarter),
                 "'open_first' is FALSE but 'x' has open interval \\[\"<2001 Q1\"\\]")
    expect_error(format_cohort_month_quarter_year(x = c("2000 Q4", "1999 Q2"),
                                                  break_min = "2000 Q2",
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_quarter_label,
                                                  break_min_lab_fun = date_to_quarter_label,
                                                  parse_fun = parse_quarters,
                                                  labels_fun = make_labels_cohort_quarter),
                 "'open_first' is FALSE but 'x' has interval \\[\"1999 Q2\"\\] that starts below 'break_min' \\[\"2000 Q2\"\\]")
})


test_that("'format_cohort_month_quarter_year' gives correct answer with valid inputs - months", {
    x <- c("2000 Feb", "2005 Dec", "1990 Jan", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                     break_min_lab_fun = date_to_month_label,
                                                     parse_fun = parse_months,
                                                     labels_fun = make_labels_cohort_month)
    ans_expected <- factor(x,
                           levels = c(paste(rep(1990:2005, each = 12),
                                            month.abb),
                                      NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2000 Feb", "2005 Dec", "1990 Jan", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = "1980 Jan",
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                     break_min_lab_fun = date_to_month_label,
                                                     parse_fun = parse_months,
                                                     labels_fun = make_labels_cohort_month)
    ans_expected <- factor(x,
                           levels = c(paste(rep(1980:2005, each = 12),
                                            month.abb),
                                      NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("2000 Feb", "2005 Dec", "1990 Jan", NA)
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = "2002 Aug",
                                                     open_first = NULL,
                                                     break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                     break_min_lab_fun = date_to_month_label,
                                                     parse_fun = parse_months,
                                                     labels_fun = make_labels_cohort_month)
    ans_expected <- factor(c("<2002 Aug", "2005 Dec", "<2002 Aug", NA),
                           levels = c("<2002 Aug", "2002 Aug", "2002 Sep",
                                      "2002 Oct", "2002 Nov", "2002 Dec",
                                      paste(rep(2003:2005, each = 12),
                                            month.abb),
                                      NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- character()
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                     break_min_lab_fun = date_to_month_label,
                                                     parse_fun = parse_months,
                                                     labels_fun = make_labels_cohort_month)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)
    x <- as.character(c(NA, NA, NA))
    ans_obtained <- format_cohort_month_quarter_year(x = x,
                                                     break_min = NULL,
                                                     open_first = FALSE,
                                                     break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                     break_min_lab_fun = date_to_month_label,
                                                     parse_fun = parse_months,
                                                     labels_fun = make_labels_cohort_month)
    ans_expected <- factor(x,
                           levels = NA,
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})


test_that("'format_cohort_month_quarter_year' throws correct error with invalid inputs - months", {
    expect_error(format_cohort_month_quarter_year(x = c("2000 Mar", "2001 Sep+"),
                                                  break_min = NULL,
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                  break_min_lab_fun = date_to_month_label,
                                                  parse_fun = parse_months,
                                                  labels_fun = make_labels_cohort_month),
                 "'x' has interval \\[\"2001 Sep\\+\"\\] that is open on the right")
    expect_error(format_cohort_month_quarter_year(x = c("2000 Jul", "<2001 Jun"),
                                                  break_min = "2000 May",
                                                  open_first = NULL,
                                                  break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                  break_min_lab_fun = date_to_month_label,
                                                  parse_fun = parse_months,
                                                  labels_fun = make_labels_cohort_month),
                 "'x' has open interval \\[\"<2001 Jun\"\\] that ends above 'break_min' \\[\"2000 May\"\\]")
    expect_error(format_cohort_month_quarter_year(x = c("2000 Feb", "<2001 Jul"),
                                                  break_min = NULL,
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                  break_min_lab_fun = date_to_month_label,
                                                  parse_fun = parse_months,
                                                  labels_fun = make_labels_cohort_month),
                 "'open_first' is FALSE but 'x' has open interval \\[\"<2001 Jul\"\\]")
    expect_error(format_cohort_month_quarter_year(x = c("2000 Nov", "1999 Apr"),
                                                  break_min = "2000 Feb",
                                                  open_first = FALSE,
                                                  break_min_tdy_fun = demcheck::err_tdy_month_label,
                                                  break_min_lab_fun = date_to_month_label,
                                                  parse_fun = parse_months,
                                                  labels_fun = make_labels_cohort_month),
                 "'open_first' is FALSE but 'x' has interval \\[\"1999 Apr\"\\] that starts below 'break_min' \\[\"2000 Feb\"\\]")
})


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
                 "'x' has open interval \\[\"2001 Q3\\+\"\\]")
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
                 "'x' has open interval \\[\"2001 Jun\\+\"\\]")
    expect_error(format_period_month_quarter_year(x = c("2000 Mar", "<2001 Jun"),
                                                  parse_fun = parse_months,
                                                  labels_fun = make_labels_period_month),
                 "'x' has open interval \\[\"<2001 Jun\"\\]")
})
  

## format_triangle_month_quarter_year -------------------------------------------

test_that("'format_triangle_month_quarter_year' gives correct answer with valid inputs", {
    ## 'break_max' supplied, and 'open_last' is TRUE 
    x <- c("Upper", "Lower", "Upper", NA, "Lower", "Lower", "Lower")
    age <- c(0, 105, 20, 108, 100, NA, "100+")
    ans_obtained <- format_triangle_month_quarter_year(x = x,
                                                       age = age,
                                                       break_max = 100,
                                                       open_last = TRUE)
    ans_expected <- factor(c("Upper", "Upper", "Upper", "Upper", "Lower", NA, "Lower"),
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## 'break_max' supplied, and 'open_last' is FALSE 
    x <- c("Upper", "Lower", "Upper", NA, "Lower", "Lower")
    age <- c(0, 105, 20, 108, 100, NA)
    ans_obtained <- format_triangle_month_quarter_year(x = x,
                                                       age = age,
                                                       break_max = 110,
                                                       open_last = TRUE)
    ans_expected <- factor(c("Upper", "Lower", "Upper", NA, "Lower", NA),
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## 'break_max' is NULL
    x <- c("Upper", "Lower", "Upper", NA, "Lower", "Lower")
    age <- c(0, 105, 20, 108, 100, NA)
    ans_obtained <- format_triangle_month_quarter_year(x = x,
                                                       age = age,
                                                       break_max = NULL,
                                                       open_last = TRUE)
    ans_expected <- factor(x,
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    ## 'x' has length 0
    x <- character()
    age <- integer()
    ans_obtained <- format_triangle_month_quarter_year(x = x,
                                                       age = age,
                                                       break_max = NULL,
                                                       open_last = TRUE)
    ans_expected <- factor(character(),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
    ## 'x' or 'age' all NA
    x <- c("Lower", NA)
    age <- c(NA, 20)
    ans_obtained <- format_triangle_month_quarter_year(x = x,
                                                       age = age,
                                                       break_max = 100,
                                                       open_last = TRUE)
    ans_expected <- factor(c(NA, NA),
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'format_triangle_month_quarter_year' throws correct error with invalid inputs - years", {
    expect_error(format_triangle_month_quarter_year(x = c("Lower", "wrong"),
                                                    age = c("10", "50"),
                                                    break_max = 100,
                                                    open_last = TRUE),
                 "'x' has invalid value for Lexis triangle \\[\"wrong\"\\]")
    expect_error(format_triangle_month_quarter_year(x = c("Lower", NA),
                                                    age = c("10", "<5"),
                                                    break_max = NULL,
                                                    open_last = TRUE),
                 "'age' has interval \\[\"<5\"\\] that is open on the left")
    expect_error(format_triangle_month_quarter_year(x = c("Lower", "Upper"),
                                                    age = c("5+", "0"),
                                                    break_max = 10,
                                                    open_last = TRUE),
                 "'age' has open interval \\[\"5\\+\"\\] that starts below 'break_max' \\[10\\]")
    expect_error(format_triangle_month_quarter_year(x = c("Lower", "Upper"),
                                                    age = c("5+", "0"),
                                                    break_max = 10,
                                                    open_last = FALSE),
                 "'open_last' is FALSE but 'age' has open interval \\[\"5\\+\"\\]")
    expect_error(format_triangle_month_quarter_year(x = c("Lower", "Upper"),
                                                    age = c("10", "0"),
                                                    break_max = 5,
                                                    open_last = FALSE),
                 "'age' has interval \\[\"10\"\\] that ends above 'break_max' \\[5\\]")
})


## make_i_interval ------------------------------------------------------------

test_that("'make_i_interval' gives correct answer with valid inputs", {
    expect_identical(make_i_interval(low = c(1990L, 2002L, 2000L, 2005L, 2006L, 2020L, NA),
                                     up = c(1995L, 2005L, 2001L, 2020L, 2019L, NA, NA),
                                     breaks = c(2000L, 2005L, 2020L),
                                     open_first = TRUE,
                                     open_last = TRUE),
                     c(1L, 2L, 2L, 3L, 3L, 4L, NA))
    expect_identical(make_i_interval(low = NA_integer_,
                                     up = 2000L, 
                                     breaks = c(2000L, 2005L, 2020L),
                                     open_first = TRUE,
                                     open_last = FALSE),
                     1L)
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


## quote_if_nonnum ---------------------------------------------------------

test_that("'quote_if_nonnum' gives correct answer with valid inputs", {
    expect_identical(quote_if_nonnum(2000),
                     "2000")
    expect_identical(quote_if_nonnum(as.Date("2000-01-01")),
                     "\"2000-01-01\"")
})
