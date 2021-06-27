
## dtabs ----------------------------------------------------------------------

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


## dtabs_survey ---------------------------------------------------------------

test_that("dtabs_survey gives same answer as xtabs when the weights are uniform", {
    d <- data.frame(y = rep(c(0, 0, 1, 0), times = 5),
                    f1 = rep(1:2, times = 10),
                    f2 = rep(c("a", "b"), each = 10),
                    wt = rep(2, times = 20))
    ans_obtained <- dtabs_survey(d, y ~ f1 + f2, weights = wt)
    ans_expected <- list(successes = as(xtabs(y ~ f1 + f2, d), "array"),
                         trials = as(xtabs(~ f1 + f2, d), "array"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("dtabs_survey gives smaller numbers for trials than xtabs when the weights vary (except for zeros)", {
    ## note that effective number of successes may be higher than original number, because of weighting
    for (seed in 1:5) {
        set.seed(seed) 
        d <- data.frame(y = rbinom(n = 20, size = 1, prob = 0.25),
                        f1 = rep(1:2, times = 10),
                        f2 = rep(c("a", "b"), each = 10),
                        wt = rep(1:5, times = 4))
        ans_obtained <- dtabs_survey(d, y ~ f1 + f2, weights = wt)
        ans_expected <- list(successes = as(xtabs(y ~ f1 + f2, d), "array"),
                             trials = as(xtabs(~ f1 + f2, d), "array"))
        expect_true(all((ans_obtained$trials < ans_expected$trials)
                        | (ans_obtained$trials == 0)))
    }
})

test_that("dtabs_survey doesn't choke when some weights are 0", {
    d <- data.frame(y = rbinom(n = 20, size = 1, prob = 0.25),
                    f1 = rep(1:2, times = 10),
                    f2 = rep(c("a", "b"), each = 10),
                    wt = rep(0:4, times = 4))
    ans_obtained <- dtabs_survey(d, y ~ f1 + f2, weights = wt)
    expect_true(!any(is.na(ans_obtained$successes)))
    expect_true(!any(is.na(ans_obtained$trials)))
})

test_that("dtabs_survey gives similar but not identical answers when method is 'gg_med' and 'gg_mean'", {
    for (seed in 1:5) {
        set.seed(seed) 
        d <- data.frame(y = rbinom(n = 20, size = 1, prob = 0.25),
                        f1 = rep(1:2, times = 10),
                        f2 = rep(c("a", "b"), each = 10),
                        wt = runif(n = 20, min = 2, max = 5))
        ans_median <- dtabs_survey(d, y ~ f1 + f2, weights = wt)
        ans_mean <- dtabs_survey(d, y ~ f1 + f2, weights = wt, method = "gg_mean")
        expect_false(identical(ans_median, ans_mean))
        expect_equal(ans_median, ans_mean, tol = 0.1)
    }
})

test_that("dtabs_survey behaves correctly when response has NA", {
    d <- data.frame(y = rep(c(0, 0, 1, 0), times = 5),
                    f1 = rep(1:2, times = 10),
                    f2 = rep(c("a", "b"), each = 10),
                    wt = rep(1:5, times = 4))
    d$y[1] <- NA
    ans_obtained <- dtabs_survey(d, y ~ f1 + f2, weights = wt) # na_rm is FALSE
    expect_true(any(is.na(ans_obtained$successes)))
    expect_true(!any(is.na(ans_obtained$trials)))
    ans_obtained <- dtabs_survey(d, y ~ f1 + f2, weights = wt, na_rm = TRUE)
    expect_true(!any(is.na(ans_obtained$successes)))
    expect_true(!any(is.na(ans_obtained$trials)))
})

test_that("dtabs_survey sets effective successes and trials to 0 when cell has no observations", {
    d <- data.frame(y = rbinom(n = 20, size = 1, prob = 0.25),
                    f1 = factor(rep(1:2, times = 10)),
                    f2 = factor(rep(c("a", "b"), each = 10)),
                    wt = rep(1:5, times = 4))
    d <- d[!(d$f1 == 1 & d$f2 == "a"), ]
    ans_obtained <- dtabs_survey(d, y ~ f1 + f2, weights = wt)
    expect_equal(ans_obtained$successes["1", "a"], 0)
    expect_equal(ans_obtained$trials["1", "a"], 0)
})

test_that("dtabs_survey sets effective successes and trials to non-0 when cell has 1 observation", {
    d <- data.frame(y = rbinom(n = 20, size = 1, prob = 0.25),
                    f1 = factor(rep(1:2, times = 10)),
                    f2 = factor(rep(c("a", "b"), each = 10)),
                    wt = rep(1:5, times = 4))
    d <- d[!(d$f1 == 1 & d$f2 == "a"), ]
    d <- rbind(d, data.frame(y = 1, f1 = 1, f2 = "a", wt = 2))
    ans_obtained <- dtabs_survey(d, y ~ f1 + f2, weights = wt)
    expect_true(ans_obtained$successes["1", "a"] > 0)
    expect_true(ans_obtained$trials["1", "a"] > 0)
})

test_that("dtabs_survey gives same answer with logical and 0-1", {
    for (seed in 1:5) {
        set.seed(seed)
        d_logical <- data.frame(y = sample(c(TRUE, FALSE), size = 20, replace = TRUE),
                                f1 = factor(rep(1:2, times = 10)),
                                f2 = factor(rep(c("a", "b"), each = 10)),
                                wt = rep(1:5, times = 4))
        d_01 <- d_logical
        d_01$y <- d_01$y * 1L
        ans_logical <- dtabs_survey(d_logical, y ~ f1 + f2, weights = wt)
        ans_01 <- dtabs_survey(d_01, y ~ f1 + f2, weights = wt)
        expect_identical(ans_logical, ans_01)
    }
})

test_that("dtabs_survey throws correct error with invalid inputs", {
    d <- data.frame(y = rep(c(0, 0, 1, 0), times = 5),
                    f1 = rep(1:10, times = 2),
                    f2 = rep(c("a", "b"), each = 10),
                    wt = rep(1:2, times = 10))
    expect_error(dtabs_survey(d, y ~ f1 + f2, weights = wt),
                 "no cells have 2 or more observations, so no design effects can be calculated")
})
