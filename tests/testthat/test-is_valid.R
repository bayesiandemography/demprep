
context("is_valid")


## is_valid_age ---------------------------------------------------------------

test_that("'is_valid_age' codes valid age groups as TRUE", {
    expect_true(is_valid_age("0"))
    expect_true(is_valid_age("0q"))
    expect_true(is_valid_age("0m"))
    expect_true(is_valid_age("1-4"))
    expect_true(is_valid_age("5+"))
    expect_true(is_valid_age("5q+"))
    expect_true(is_valid_age("5m+"))
})

test_that("'is_valid_age' codes invalid age groups as TRUE", {
    expect_false(is_valid_age("<5"))
    expect_false(is_valid_age("1-1"))
    expect_false(is_valid_age("2-1"))
    expect_false(is_valid_age("wrong"))
    expect_false(is_valid_age("0qm"))
    expect_false(is_valid_age("0-4m"))
    expect_false(is_valid_age(NA))
})

test_that("'is_valid_age' works with vectors with length > 1", {
    expect_identical(is_valid_age(c("<5", "5+")),
                     c(FALSE, TRUE))
})

test_that("'is_valid_age' works with vectors with length 0", {
    expect_identical(is_valid_age(character()),
                     logical())
})


    

