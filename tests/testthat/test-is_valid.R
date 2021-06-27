
## is_valid_age ---------------------------------------------------------------

test_that("'is_valid_age' codes valid age groups as TRUE", {
    expect_true(is_valid_age("0"))
    expect_true(is_valid_age("1-4"))
    expect_true(is_valid_age("5+"))
    expect_true(is_valid_age(NA))
})

test_that("'is_valid_age' codes invalid age groups as FALSE", {
    expect_false(is_valid_age("<5"))
    expect_false(is_valid_age("1-1"))
    expect_false(is_valid_age("2-1"))
    expect_false(is_valid_age("wrong"))
    expect_false(is_valid_age("0q"))
    expect_false(is_valid_age("4m"))
})

test_that("'is_valid_age' works with vectors with length > 1", {
    expect_identical(is_valid_age(c("<5", "5+")),
                     c(FALSE, TRUE))
})

test_that("'is_valid_age' works with vectors with length 0", {
    expect_identical(is_valid_age(character()),
                     logical())
})


## is_valid_cohort ------------------------------------------------------------

test_that("'is_valid_cohort' codes valid cohorts as TRUE", {
    expect_true(is_valid_cohort("2000"))
    expect_true(is_valid_cohort("2000-2005"))
    expect_true(is_valid_cohort("<2000"))
    expect_true(is_valid_cohort("2000 Q3"))
    expect_true(is_valid_cohort("<2000 Q3"))
    expect_true(is_valid_cohort("2000 Mar"))
    expect_true(is_valid_cohort("<2000 Mar"))
    expect_true(is_valid_cohort(NA))
})

test_that("'is_valid_cohort' codes invalid cohorts as FALSE", {
    expect_false(is_valid_cohort("2000+"))
    expect_false(is_valid_cohort("2021-2021"))
    expect_false(is_valid_cohort("2021-2020"))
    expect_false(is_valid_cohort("wrong"))
    expect_false(is_valid_cohort("2000 Q3+"))
    expect_false(is_valid_cohort("2000 Jan+"))
    expect_false(is_valid_cohort("2000 Q5+"))
    expect_false(is_valid_cohort("2000 Wro+"))
})

test_that("'is_valid_cohort' works with vectors with length > 1", {
    expect_identical(is_valid_cohort(c("2000", "<2000", "2000+")),
                     c(TRUE, TRUE, FALSE))
})

test_that("'is_valid_cohort' works with vectors with length 0", {
    expect_identical(is_valid_cohort(character()),
                     logical())
})


## is_valid_period ------------------------------------------------------------

test_that("'is_valid_period' codes valid periods as TRUE", {
    expect_true(is_valid_period("2000"))
    expect_true(is_valid_period("2000-2005"))
    expect_true(is_valid_period("2000 Q3"))
    expect_true(is_valid_period("2000 Mar"))
    expect_true(is_valid_period(NA))
})

test_that("'is_valid_period' codes invalid periods as FALSE", {
    expect_false(is_valid_period("<2000"))
    expect_false(is_valid_period("<2000 Q3"))
    expect_false(is_valid_period("<2000 Mar"))
    expect_false(is_valid_period("2000+"))
    expect_false(is_valid_period("2021-2021"))
    expect_false(is_valid_period("2021-2020"))
    expect_false(is_valid_period("wrong"))
    expect_false(is_valid_period("2000 Q3+"))
    expect_false(is_valid_period("2000 Jan+"))
    expect_false(is_valid_period("2000 Q5+"))
    expect_false(is_valid_period("2000 Wro+"))
})

test_that("'is_valid_period' works with vectors with length > 1", {
    expect_identical(is_valid_period(c("2000", "<2000", "2000+")),
                     c(TRUE, FALSE, FALSE))
})

test_that("'is_valid_period' works with vectors with length 0", {
    expect_identical(is_valid_period(character()),
                     logical())
})


    

