
context("make_labels_period")

## make_labels_period_year ----------------------------------------------------

test_that("make_labels_period_year gives correct answers with valid input", {
    expect_identical(make_labels_period_year(breaks = as.Date(c("2000-01-01", "2001-01-01", "2002-01-01"))),
                     c("2000", "2001"))
    expect_identical(make_labels_period_year(breaks = as.Date(c("2000-01-01", "2001-01-01", "2002-01-01")),
                                             year_to = FALSE),
                     c("2000", "2001"))
    expect_identical(make_labels_period_year(breaks = as.Date(c("2000-02-01", "2001-02-01", "2002-02-01"))),
                     c("2001", "2002"))
    expect_identical(make_labels_period_year(breaks = as.Date(c("2000-02-01", "2001-02-01", "2002-02-01")),
                                             year_to = FALSE),
                     c("2000", "2001"))
    expect_identical(make_labels_period_year(breaks = as.Date(c("2001-07-01", "2006-07-01", "2011-07-01"))),
                     c("2001-2006", "2006-2011"))
    expect_identical(make_labels_period_year(breaks = as.Date(c("2001-07-01", "2006-07-01", "2011-07-01")),
                                             year_to = FALSE),
                     c("2001-2006", "2006-2011"))
    expect_identical(make_labels_period_year(breaks = as.Date(c("2001-07-01", "2006-07-01", "2011-07-01")),
                                             open_left = TRUE,
                                             open_right = TRUE,
                                             include_na = TRUE),
                     c("<2001", "2001-2006", "2006-2011", "2011+", NA))
    expect_identical(make_labels_period_year(breaks = as.Date(character())),
                     character())
})

test_that("make_labels_period_year throws correct error with invalid input", {
    expect_error(make_labels_period_year(breaks = character(),
                                         open_left = TRUE,
                                         open_right = FALSE),
                 "'breaks' has length 0 but 'open_left' is TRUE")
    expect_error(make_labels_period_year(breaks = character(),
                                         open_left = FALSE,
                                         open_right = TRUE),
                 "'breaks' has length 0 but 'open_right' is TRUE")
})


## ## make_labels_period_quarter --------------------------------------------------

## test_that("make_labels_period_quarter gives correct answers with valid input", {
##     expect_identical(make_labels_period_quarter(),
##                      c(paste0(0:399, "q"), "400q+"))
##     expect_identical(make_labels_period_quarter(max_break = 5,
##                                                    open_left = TRUE,
##                                                    open_right = FALSE,
##                                                    include_na = TRUE),
##                      c("<0q", "0q", "1q", "2q", "3q", "4q", NA))
## })


## ## make_labels_period_month --------------------------------------------------

## test_that("make_labels_period_month gives correct answers with valid input", {
##     expect_identical(make_labels_period_month(),
##                      c(paste0(0:1199, "m"), "1200m+"))
##     expect_identical(make_labels_period_month(max_break = 5,
##                                                  open_left = TRUE,
##                                                  open_right = FALSE,
##                                                  include_na = TRUE),
##                      c("<0m", "0m", "1m", "2m", "3m", "4m", NA))
## })
