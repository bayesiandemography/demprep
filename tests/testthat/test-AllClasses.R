
## Further tests in "test-Labels-generators.R"

context("AllClasses")

test_that("validity function for LabCategories throws correct error", {
    x <- LabCategories(labels = c("a", "b", "c"),
                       include_na = TRUE)
    expect_is(x, "LabCategories")
    x_wrong <- x
    x_wrong@labels[[1L]] <- NA
    expect_error(validObject(x_wrong),
                 "'labels' has NAs")
    x_wrong <- x
    x_wrong@include_na <- NA
    expect_error(validObject(x_wrong),
                 "'include_na' is NA")
})

test_that("validity function for LabTriangles throws correct error", {
    x <- LabTriangles(include_na = FALSE)
    expect_is(x, "LabTriangles")
    x_wrong <- x
    x_wrong@labels[[1L]] <- "wrong"
    expect_error(validObject(x_wrong),
                 "'labels' \\[\"wrong\", \"Upper\"\\] not identical to \"Lower\", \"Upper\"")
})

test_that("validity function for LabPool throws correct error", {
    x <- LabPool(include_na = FALSE)
    expect_is(x, "LabPool")
    x_wrong <- x
    x_wrong@labels[[1L]] <- "wrong"
    expect_error(validObject(x_wrong),
                 "'labels' \\[\"wrong\", \"Outs\"\\] not identical to \"Ins\", \"Outs\"")
})

test_that("validity function for LabQuantiles throws correct error", {
    x <- LabQuantiles(labels = c("2.5%", "50%", "97.5%"),
                      include_na = FALSE)
    expect_is(x, "LabQuantiles")
    x_wrong <- x
    x_wrong@labels[[1L]] <- "wrong"
    expect_error(validObject(x_wrong),
                 "\"wrong\" is not a valid quantile")
})

test_that("validity function for LabIntegers throws correct errors", {
    x <- LabIntegers(int_min = 0L,
                     int_max = 100L,
                     include_na = FALSE)
    expect_is(x, "LabIntegers")
    x_wrong <- x
    x_wrong@int_max <- -10L
    expect_error(validObject(x_wrong),
                 "'int_max' \\[-10\\] less than 'int_min' \\[0\\]")
})

test_that("validity function for LabGroupedIntEnumerations throws correct error", {
    x <- LabGroupedIntEnumerations(breaks = integer(),
                                   open_first = FALSE,
                                   open_last = FALSE,
                                   include_na = FALSE)
    expect_is(x, "LabGroupedIntEnumerations")
    x_wrong <- x
    x_wrong@open_first <- TRUE
    expect_error(validObject(x_wrong),
                 "'breaks' has length 0 but 'open_first' is TRUE")
    x_wrong <- x
    x_wrong@open_last <- TRUE
    expect_error(validObject(x_wrong),
                 "'breaks' has length 0 but 'open_last' is TRUE")
    x_wrong <- x
    x_wrong@breaks <- 1L
    expect_error(validObject(x_wrong),
                 "'breaks' has length 1 but 'open_first' and 'open_last' are both FALSE")
})


test_that("validity function for LabGroupedIntEndpoints throws correct error", {
    x <- LabGroupedIntEndpoints(breaks = integer(),
                                open_first = FALSE,
                                include_na = FALSE)
    expect_is(x, "LabGroupedIntEndpoints")
    x_wrong <- x
    x_wrong@open_first <- TRUE
    expect_error(validObject(x_wrong),
                 "'breaks' has length 0 but 'open_first' is TRUE")
    x_wrong <- x
    x_wrong@breaks <- 1L
    expect_error(validObject(x_wrong),
                 "'breaks' has length 1 but 'open_first' is FALSE")
})


test_that("validity function for LabCalendarQuarters throws correct error", {
    x <- LabCalendarQuarters(break_min = as.Date("2000-04-01"),
                             break_max = as.Date("2002-01-01"),
                             open_first = TRUE,
                             include_na = FALSE)
    expect_is(x, "LabCalendarQuarters")
    x_wrong <- x
    x_wrong@break_max <- as.Date("1999-10-01")
    expect_error(validObject(x_wrong),
                 "'break_max' \\[\"1999-10-01\"\\] less than 'break_min' \\[\"2000-04-01\"\\]")
})

test_that("validity function for LabDurationsQuarters throws correct error", {
    x <- LabDurationsQuarters(break_min = 100L,
                              break_max = 120L,
                              open_last = TRUE,
                              include_na = FALSE)
    expect_is(x, "LabDurationsQuarters")
    x_wrong <- x
    x_wrong@break_max <- 80L
    expect_error(validObject(x_wrong),
                 "'break_max' \\[80\\] less than 'break_min' \\[100\\]")
})


