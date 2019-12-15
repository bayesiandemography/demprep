
context("Labels-generators")

test_that("LabCategories creates correct object", {
    x <- LabCategories(labels = c("a", "b", "c"),
                       include_na = TRUE)
    expect_is(x, "LabCategories")
})

test_that("LabTriangles creates correct object", {
    x <- LabTriangles(include_na = FALSE)
    expect_is(x, "LabTriangles")
})

test_that("LabPool creates correct object", {
    x <- LabPool(include_na = FALSE)
    expect_is(x, "LabPool")
})

test_that("LabQuantiles creates correct object", {
    x <- LabQuantiles(labels = c("2.5%", "50%", "97.5%"),
                      include_na = FALSE)
    expect_is(x, "LabQuantiles")
    x <- LabQuantiles(labels = character(),
                      include_na = TRUE)
    expect_is(x, "LabQuantiles")
})

test_that("LabIntegers creates correct object", {
    x <- LabIntegers(int_min = 0L,
                     int_max = 100L,
                     include_na = FALSE)
    expect_is(x, "LabIntegers")
    x <- LabIntegers(int_min = 10L,
                     int_max = 10L,
                     include_na = TRUE)
    expect_is(x, "LabIntegers")
    x <- LabIntegers(int_min = 10L,
                     int_max = 10L,
                     include_na = TRUE)
    expect_is(x, "LabIntegers")
})

test_that("LabGroupedIntEnumerations creates correct object", {
    x <- LabGroupedIntEnumerations(breaks = c(-10L, 0L, 5L, 100L),
                                   open_first = TRUE,
                                   open_last = TRUE,
                                   include_na = FALSE)
    expect_is(x, "LabGroupedIntEnumerations")
    x <- LabGroupedIntEnumerations(breaks = integer(),
                                   open_first = FALSE,
                                   open_last = FALSE,
                                   include_na = TRUE)
    expect_is(x, "LabGroupedIntEnumerations")
})

test_that("LabGroupedIntEndpoints creates correct object", {
    x <- LabGroupedIntEndpoints(breaks = c(-10L, 0L, 5L, 100L),
                                open_first = TRUE,
                                include_na = FALSE)
    expect_is(x, "LabGroupedIntEndpoints")
    x <- LabGroupedIntEndpoints(breaks = integer(),
                                open_first = FALSE,
                                include_na = TRUE)
    expect_is(x, "LabGroupedIntEndpoints")
})

test_that("LabCalendarQuarters creates correct object", {
    x <- LabCalendarQuarters(break_min = as.Date("2000-04-01"),
                             break_max = as.Date("2002-01-01"),
                             open_first = TRUE,
                             include_na = FALSE)
    expect_is(x, "LabCalendarQuarters")
    x <- LabCalendarQuarters(break_min = as.Date("2000-04-01"),
                             break_max = as.Date("2000-04-01"),
                             open_first = FALSE,
                             include_na = TRUE)
    expect_is(x, "LabCalendarQuarters")
})

test_that("LabCalendarMonths creates correct object", {
    x <- LabCalendarMonths(break_min = as.Date("2000-05-01"),
                           break_max = as.Date("2002-01-01"),
                           open_first = TRUE,
                           include_na = FALSE)
    expect_is(x, "LabCalendarMonths")
    x <- LabCalendarMonths(break_min = as.Date("2000-05-01"),
                           break_max = as.Date("2000-05-01"),
                           open_first = FALSE,
                           include_na = TRUE)
    expect_is(x, "LabCalendarMonths")
})

test_that("LabDurationsQuarters creates correct object", {
    x <- LabDurationsQuarters(break_min = 0L,
                              break_max = 120L,
                              open_last = TRUE,
                              include_na = FALSE)
    expect_is(x, "LabDurationsQuarters")
    x <- LabDurationsQuarters(break_min = 10L,
                              break_max = 10L,
                              open_last = FALSE,
                              include_na = TRUE)
    expect_is(x, "LabDurationsQuarters")
})

test_that("LabDurationsMonths creates correct object", {
    x <- LabDurationsMonths(break_min = 0L,
                            break_max = 120L,
                            open_last = TRUE,
                            include_na = FALSE)
    expect_is(x, "LabDurationsMonths")
    x <- LabDurationsMonths(break_min = 10L,
                            break_max = 10L,
                            open_last = FALSE,
                            include_na = TRUE)
    expect_is(x, "LabDurationsMonths")
})





