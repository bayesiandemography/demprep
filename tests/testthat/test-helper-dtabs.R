
## make_fill ------------------------------------------------------------------

test_that("'make_fill' gives correct answer with valid inputs", {
    X <- 1:5
    INDEX <- data.frame(a = factor(5:1))
    expect_identical(make_fill(fill = 0L,
                               X = X,
                               INDEX = INDEX),
                     0L)
    expect_identical(make_fill(fill = 0,
                               X = X,
                               INDEX = INDEX),
                     0L)
    expect_identical(make_fill(fill = 999,
                               X = X,
                               INDEX = INDEX),
                     999L)
    expect_identical(make_fill(fill = NA,
                               X = X,
                               INDEX = INDEX),
                     NA_integer_)
    expect_identical(make_fill(fill = NULL,
                               X = X,
                               INDEX = INDEX),
                     0L)
    expect_identical(make_fill(fill = NULL,
                               X = -X,
                               INDEX = INDEX),
                     NA_integer_)
    expect_identical(make_fill(fill = NULL,
                               X = rep(0.1, 5),
                               INDEX = INDEX),
                     0L)
})

test_that("'make_fill' gives correct error with invalid inputs", {
    X <- c(0.1, 0.3, 0.2)
    INDEX <- data.frame(a = factor(c(1, 2, 1)), b = factor(c(10, 10, 11)))
    expect_error(make_fill(fill = NULL,
                           X = X,
                           INDEX = INDEX),
                 paste("some combinations of the cross-classifying variables are not included",
                       "in the data, but no value for 'fill' has been supplied"))
    expect_error(make_fill(fill = "a",
                           X = X,
                           INDEX = INDEX),
                 "invalid value for 'fill'")
})
