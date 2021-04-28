
context("labels_to_dates")

## labels_to_dates_month ----------------------------------------------------

test_that("labels_to_dates_month gives correct answers with valid inputs", {
    expect_identical(labels_to_dates_month(x = c("2000 Feb", NA, "<1000 Nov"),
                                             name = "x"),
                     list(as.Date(c("2000-02-01", "2000-03-01")),
                          as.Date(c(NA, NA)),
                          as.Date(c(NA, "1000-11-01"))))
    expect_identical(labels_to_dates_month(x = character(),
                                             name = "x"),
                     list())
})

test_that("labels_to_dates_month throws correct errors with invalid inputs", {
    expect_error(labels_to_dates_month(x = "2000 January",
                                         name = "x"),
                 "\"2000 January\" in 'x' is not a valid label")
})


## labels_to_dates_quarter ----------------------------------------------------

test_that("labels_to_dates_quarter gives correct answers with valid inputs", {
    expect_identical(labels_to_dates_quarter(x = c("2000 Q1", NA, "<1000 Q4"),
                                             name = "x"),
                     list(as.Date(c("2000-01-01", "2000-04-01")),
                          as.Date(c(NA, NA)),
                          as.Date(c(NA, "1000-10-01"))))
    expect_identical(labels_to_dates_quarter(x = character(),
                                             name = "x"),
                     list())
})

test_that("labels_to_dates_quarter throws correct errors with invalid inputs", {
    expect_error(labels_to_dates_quarter(x = "2000 Q5",
                                         name = "x"),
                 "\"2000 Q5\" in 'x' is not a valid label")
})
    

    
