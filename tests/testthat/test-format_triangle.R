
context("format_triangle")

## format_triangle_year -------------------------------------------------------

test_that("format_triangle_year gives correct answers with default values", {
    x <- c("Lower", "Upper", "Lower", NA, "Lower", NA)
    age <- c(0, 100, 100, 100, 101, 101)
    ans_obtained <- format_triangle_year(x = x,
                                         age = age)
    ans_expected <- factor(c("Lower", "Upper", "Lower", NA, "Upper", "Upper"),
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})



## format_triangle_multi ------------------------------------------------------

test_that("format_triangle_multi gives correct answers with default values", {
    x <- c("Lower", "Upper", "Lower", NA, "Lower", NA)
    age <- c(0, 100, 100, 100, 101, 101)
    period <- c(2000, 2001, 2002, 2003, 2004, 2005)
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Lower", "Lower", "Lower", "Lower", "Lower", "Upper"),
                           levels = c("Lower", "Upper"),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
    x <- c("Lower", "Upper", "Lower", NA, "Lower", NA)
    age <- c("0-4", "101-103", "100-104", "100+", "10", "101")
    period <- c(2000, 2001, 2002, 2003, 2004, 2005)
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Lower", "Lower", "Lower", "Lower", "Lower", "Upper"),
                           levels = c("Lower", "Upper"),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})



                                          

    
