
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

test_that("format_triangle_multi gives correct answer - triangles on the diagonal", {
    x <- c("Lower", "Upper", "Lower", "Upper")
    age <- c("0", "50-54", "21-23", "34")
    period <- c("2000", "2000-2005", "2001-2004", "2004-2005")
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Lower", "Upper", "Lower", "Upper"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_multi gives correct answer - lower below the diagonal", {
    x <- c("Lower", "Lower", "Lower")
    age <- c("0", "50-53", "21-23")
    period <- c("2001", "2001-2005", "2002-2005")
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Lower", "Lower", "Lower"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_multi gives correct answer - upper above the diagonal", {
    x <- c("Upper", "Upper", "Upper")
    age <- c("2", "52-54", "23")
    period <- c("2000", "2001-2004", "2001-2002")
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Upper", "Upper", "Upper"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_multi gives correct answer - upper below the diagonal", {
    x <- c("Upper", "Upper", "Upper")
    age <- c("0", "51-52", "20-21")
    period <- c("2001", "2003-2005", "2002-2004")
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Lower", "Lower", "Lower"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_multi gives correct answer - lower above the diagonal", {
    x <- c("Lower", "Lower", "Lower")
    age <- c("2", "53-54", "54")
    period <- c("2000", "2000-2002", "2001-2002")
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Upper", "Upper", "Upper"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_multi gives correct answer - above break_max + width", {
    x <- c("Lower", "Upper", "Lower", NA)
    age <- c("105", "105", "110+", "106")
    period <- c("2000", "2002", "2000-2005", "2003")
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Upper", "Upper", "Upper", "Upper"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_multi gives correct answer - NAs", {
    x <- c("Lower", NA, NA, "Upper")
    age <- c(NA, "105", "110+", "40")
    period <- c("2000", NA, "2000-2005", NA)
    ans_obtained <- format_triangle_multi(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c(NA, "Upper", "Upper", NA),
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_multi throws correct errors with invaid inputs", {
    expect_error(format_triangle_multi(x = "Upper",
                                       age = "<5",
                                       period = "2000-2005"),
                 "'age' has interval \\[\"<5\"\\] that is open on the left")
    expect_error(format_triangle_multi(x = "Upper",
                                       age = "5-9",
                                       period = "2000+"),
                 "'period' has open interval \\[\"2000\\+\"\\]")
    expect_error(format_triangle_multi(x = "Upper",
                                       age = "100+",
                                       period = "2000-2005",
                                       open_last = FALSE),
                 "'open_last' is FALSE but 'age' has open interval \\[\"100\\+\"\\]")
    expect_error(format_triangle_multi(x = "Upper",
                                       age = "95-104",
                                       period = "2000-2005",
                                       open_last = FALSE),
                 "'age' has interval \\[\"95-104\"\\] that ends above 'break_max' \\[100\\]")
    expect_error(format_triangle_multi(x = "Upper",
                                       age = "85-94",
                                       period = "2000-2005"),
                 "'age' has interval \\[\"85-94\"\\] that intersects two or more intervals formed using 'break_max = 100' and 'width = 5'")
    expect_error(format_triangle_multi(x = "Upper",
                                       age = "85-87",
                                       period = "2003-2006"),
                 "'period' has interval \\[\"2003-2006\"\\] that intersects two or more intervals formed using 'origin = 2000' and 'width = 5'")
    expect_error(format_triangle_multi(x = "Upper",
                                       age = "85-87",
                                       period = "2000-2005"),
                 "element 1 of 'age' \\[\"85-87\"\\] and element 1 of 'period' \\[\"2000-2005\"\\] have different widths, so do not form a Lexis square")
    expect_error(format_triangle_multi(x = "Lower",
                                       age = "81-84",
                                       period = "2000-2004"),
                 "element 1 of 'x' \\[\"Lower\"\\], for which 'age' is \"81-84\" and 'period' is \"2000-2004\", falls within two or more newly-created Lexis triangles")
})


## format_triangle_births -----------------------------------------------------

test_that("format_triangle_births gives correct answer - triangles on the diagonal", {
    x <- c("Lower", "Upper", "Lower", "Upper")
    age <- c("20", "40-44", "21-23", "34")
    period <- c("2000", "2000-2005", "2001-2004", "2004-2005")
    ans_obtained <- format_triangle_births(x = x,
                                           age = age,
                                           period = period)
    ans_expected <- factor(c("Lower", "Upper", "Lower", "Upper"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_births gives correct answer - lower below the diagonal", {
    x <- c("Lower", "Lower", "Lower")
    age <- c("20", "40-43", "21-23")
    period <- c("2001", "2001-2005", "2002-2005")
    ans_obtained <- format_triangle_births(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Lower", "Lower", "Lower"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_births gives correct answer - upper above the diagonal", {
    x <- c("Upper", "Upper", "Upper")
    age <- c("22", "32-34", "23")
    period <- c("2000", "2001-2004", "2001-2002")
    ans_obtained <- format_triangle_births(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c("Upper", "Upper", "Upper"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_births gives correct answer - upper below the diagonal", {
    x <- c("Upper", "Upper", "Upper")
    age <- c("40", "31-32", "20-21")
    period <- c("2001", "2003-2005", "2002-2004")
    ans_obtained <- format_triangle_births(x = x,
                                          age = age,
                                          period = period,
                                          break_min = NULL,
                                          break_max = NULL)
    ans_expected <- factor(c("Lower", "Lower", "Lower"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_births gives correct answer - lower above the diagonal", {
    x <- c("Lower", "Lower", "Lower")
    age <- c("12", "53-54", "54")
    period <- c("2000", "2000-2002", "2001-2002")
    ans_obtained <- format_triangle_births(x = x,
                                          age = age,
                                          period = period,
                                          break_min = 10,
                                          break_max = 55)
    ans_expected <- factor(c("Upper", "Upper", "Upper"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_births gives correct answer - recoding up", {
    x <- c("Lower", "Upper", NA)
    age <- c("10", "10-12", "10-14")
    period <- c("2000", "2002-2005", "2000-2005")
    ans_obtained <- format_triangle_births(x = x,
                                          age = age,
                                          period = period,
                                          recode_up = TRUE)
    ans_expected <- factor(c("Lower", "Lower", "Lower"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_births gives correct answer - recoding up", {
    x <- c("Lower", "Upper", NA)
    age <- c("50", "60-62", "50-54")
    period <- c("2000", "2002-2005", "2000-2005")
    ans_obtained <- format_triangle_births(x = x,
                                          age = age,
                                          period = period,
                                          recode_down = TRUE)
    ans_expected <- factor(c("Upper", "Upper", "Upper"),
                           levels = c("Lower", "Upper"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_births gives correct answer - NAs", {
    x <- c("Lower", NA, NA, "Upper")
    age <- c(NA, "35", "20-24", "40")
    period <- c("2000", NA, "2000-2005", NA)
    ans_obtained <- format_triangle_births(x = x,
                                          age = age,
                                          period = period)
    ans_expected <- factor(c(NA, NA, NA, NA),
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("format_triangle_births throws correct errors with invaid inputs", {
    expect_error(format_triangle_births(x = "Upper",
                                       age = "<25",
                                       period = "2000-2005"),
                 "'age' has open interval \\[\"<25\"\\]")
    expect_error(format_triangle_births(x = "Upper",
                                       age = "25-29",
                                       period = "2000+"),
                 "'period' has open interval \\[\"2000\\+\"\\]")
    expect_error(format_triangle_births(x = "Upper",
                                       age = "10-14",
                                       period = "2000-2005"),
                 "'age' has interval \\[\"10-14\"\\] that starts below 'break_min' \\[15\\] and 'recode_up' is FALSE")
    expect_error(format_triangle_births(x = "Upper",
                                       age = "80-84",
                                       period = "2000-2005"),
                 "'age' has interval \\[\"80-84\"\\] that ends above 'break_max' \\[50\\] and 'recode_down' is FALSE")
    expect_error(format_triangle_births(x = "Upper",
                                       age = "29-31",
                                       period = "2000-2003"),
                 "'age' has interval \\[\"29-31\"\\] that intersects two or more intervals formed using 'break_min = 15', 'break_max = 50', and 'width = 5'")
    expect_error(format_triangle_births(x = "Upper",
                                       age = "35-37",
                                       period = "2003-2006"),
                 "'period' has interval \\[\"2003-2006\"\\] that intersects two or more intervals formed using 'origin = 2000' and 'width = 5'")
    expect_error(format_triangle_births(x = "Upper",
                                       age = "25-27",
                                       period = "2000-2005"),
                 "element 1 of 'age' \\[\"25-27\"\\] and element 1 of 'period' \\[\"2000-2005\"\\] have different widths, so do not form a Lexis square")
    expect_error(format_triangle_births(x = "Lower",
                                       age = "21-24",
                                       period = "2000-2004"),
                 "element 1 of 'x' \\[\"Lower\"\\], for which 'age' is \"21-24\" and 'period' is \"2000-2004\", falls within two or more newly-created Lexis triangles")
})


## format_triangle_quarter ----------------------------------------------------

test_that("format_triangle_quarter gives correct answers with default values", {
    x <- c("Lower", "Upper", "Lower", NA, "Lower", NA)
    age <- c(0, 400, 400, 400, 401, 401)
    ans_obtained <- format_triangle_quarter(x = x,
                                            age = age)
    ans_expected <- factor(c("Lower", "Upper", "Lower", NA, "Upper", "Upper"),
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})


## format_triangle_month ------------------------------------------------------

test_that("format_triangle_month gives correct answers with default values", {
    x <- c("Lower", "Upper", "Lower", NA, "Lower", NA)
    age <- c(0, 1200, 1200, 1200, 1201, 1201)
    ans_obtained <- format_triangle_month(x = x,
                                          age = age)
    ans_expected <- factor(c("Lower", "Upper", "Lower", NA, "Upper", "Upper"),
                           levels = c("Lower", "Upper", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})
