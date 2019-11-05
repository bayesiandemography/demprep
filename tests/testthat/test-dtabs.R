
context("dtabs")

test_that("dtabs gives same answer as xtabs", {
    d <- data.frame(y = 1:20,
                    f1 = rep(1:10, times = 2),
                    f2 = rep(c("a", "b"), each = 10))
    formula <- y ~ f1 + f2
    ans_obtained <- dtabs(d, formula)
    ans_expected <- as(xtabs(formula, d), "array")
    expect_identical(ans_obtained, ans_expected)
    formula <-  ~ f1 + f2
    ans_obtained <- dtabs(d, formula)
    ans_expected <- as(xtabs(formula, d), "array")
    expect_identical(ans_obtained, ans_expected)
    formula <-  ~ f1
    ans_obtained <- dtabs(d, formula)
    ans_expected <- as(xtabs(formula, d), "array")
    expect_identical(ans_obtained, ans_expected)
    formula <- y ~ .
    ans_obtained <- dtabs(d, formula)
    ans_expected <- as(xtabs(formula, d), "array")
    expect_identical(ans_obtained, ans_expected)
    formula <-  ~ .
    ans_obtained <- dtabs(d, formula)
    ans_expected <- as(xtabs(formula, d), "array")
    expect_identical(ans_obtained, ans_expected)
    a <- letters[1:10]
    b <- rep(c("x", "y"), each = 5)
    ans_obtained <- dtabs(formula = ~ a + b)
    ans_expected <- as(xtabs(~ a + b), "array")
    expect_identical(ans_obtained, ans_expected)
})

test_that("fill answer for dtabs works correctly", {
    d <- data.frame(y = 1:20,
                    f1 = rep(1:10, times = 2),
                    f2 = rep(c("a", "b"), each = 10))
    formula <- y ~ f1 + f2
    d_miss <- d[-20,]
    ans_obtained <- dtabs(d_miss, formula)
    ans_expected <- dtabs(d, formula)
    ans_expected[20] <- 0L
    expect_identical(ans_obtained, ans_expected)
    formula <- y ~ f1 + f2
    d_miss <- d[-20,]
    ans_obtained <- dtabs(d_miss, formula, fill = NA)
    ans_expected <- dtabs(d, formula)
    ans_expected[20] <- NA
    expect_identical(ans_obtained, ans_expected)
    formula <- y ~ f1
    d_miss <- d[-20,]
    ans_obtained <- dtabs(d_miss, formula, fill = NA)
    ans_expected <- dtabs(d, formula)
    ans_expected[10] <- 10L
    expect_identical(ans_obtained, ans_expected)
    formula <- y ~ f1
    d_miss <- d
    d_miss$f1 <- factor(d_miss$f1)
    d_miss <- d_miss[-c(10, 20),]
    ans_obtained <- dtabs(d_miss, formula, fill = 0L)
    ans_expected <- dtabs(d, formula)
    ans_expected[10] <- 0L
    expect_identical(ans_obtained, ans_expected)
    d_miss <- d
    d_miss$f1 <- factor(d_miss$f1)
    d_miss <- d_miss[-c(10, 20),]
    ans_obtained <- dtabs(d_miss, formula, fill = NA)
    ans_expected <- dtabs(d, formula)
    ans_expected[10] <- NA
    expect_identical(ans_obtained, ans_expected)
})

