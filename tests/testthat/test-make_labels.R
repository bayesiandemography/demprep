
context("make_labels")

## Categories -----------------------------------------------------------------

test_that("'make_labels_categories' gives correct answer with valid inputs", {
    x <- c("a", "b", "c", NA)
    expect_identical(make_labels_categories(x), x)
    x <- character()
    expect_identical(make_labels_categories(x), x)
    x <- NA_character_
    expect_identical(make_labels_categories(x), x)
})


## make_labels_triangles ------------------------------------------------------

test_that("'make_labels_triangles' gives correct answer with valid inputs", {
    x <- c("Lower", "Upper", NA)
    expect_identical(make_labels_triangles(x), x)
    x <- c(NA, "Upper")
    expect_identical(make_labels_triangles(x), x)
    x <- character()
    expect_identical(make_labels_triangles(x), x)
    x <- NA_character_
    expect_identical(make_labels_triangles(x), x)
})


## make_labels_directions ------------------------------------------------------

test_that("'make_labels_directions' gives correct answer with valid inputs", {
    x <- c("In", "Out", NA)
    expect_identical(make_labels_directions(x), x)
    x <- c(NA, "Out")
    expect_identical(make_labels_directions(x), x)
    x <- character()
    expect_identical(make_labels_directions(x), x)
    x <- NA_character_
    expect_identical(make_labels_directions(x), x)
})


## make_labels_quantiles ------------------------------------------------------

test_that("'make_labels_quantiles' gives correct answer with valid inputs", {
    x <- c("50%", "1.2%", NA)
    expect_identical(make_labels_quantiles(x), x)
    x <- c(NA, "100%")
    expect_identical(make_labels_quantiles(x), x)
    x <- character()
    expect_identical(make_labels_quantiles(x), x)
    x <- NA_character_
    expect_identical(make_labels_quantiles(x), x)
})


## make_labels_integers -------------------------------------------------------

test_that("'make_labels_integers' gives correct answer with valid inputs", {
    x <- list(c(50L, 51L),
              c(NA_integer_, NA_integer_),
              c(NA, 0L),
              c(100L, NA))
    expect_identical(make_labels_integers(x),
                     c("50", NA, "<0", "100+"))
    x <- list()
    expect_identical(make_labels_integers(x),
                     character())
    x <- list(c(NA_integer_, NA_integer_))
    expect_identical(make_labels_integers(x),
                     NA_character_)
})


## make_labels_intervals -------------------------------------------------------

test_that("'make_labels_intervals' gives correct answer with valid inputs", {
    x <- list(c(50L, 51L),
              c(60L, 70L),
              c(NA_integer_, NA_integer_),
              c(NA, 0L),
              c(100L, NA))
    expect_identical(make_labels_intervals(x),
                     c("50-51", "60-70", NA, "<0", "100+"))
    x <- list()
    expect_identical(make_labels_intervals(x),
                     character())
    x <- list(c(NA_integer_, NA_integer_))
    expect_identical(make_labels_intervals(x),
                     NA_character_)
})


## make_labels_quantities -----------------------------------------------------

test_that("'make_labels_quantities' gives correct answer with valid inputs", {
    x <- list(c(50L, 51L),
              c(60L, 70L),
              c(NA_integer_, NA_integer_),
              c(NA, 0L),
              c(100L, NA))
    expect_identical(make_labels_quantities(x),
                     c("50", "60-69", NA, "<0", "100+"))
    x <- list()
    expect_identical(make_labels_quantities(x),
                     character())
    x <- list(c(NA_integer_, NA_integer_))
    expect_identical(make_labels_quantities(x),
                     NA_character_)
})


## make_labels_quarters -------------------------------------------------------

test_that("'make_labels_quarters' gives correct answer with valid inputs", {
    x <- list(as.Date(c("2020-04-01", "2020-07-01")),
              as.Date(c(NA, "2020-01-01")),
              as.Date(c("2020-10-01", NA)),
              as.Date(c(NA, NA)))
    expect_identical(make_labels_quarters(x),
                     c("2020 Q2", "<2020 Q1", "2020 Q4+", NA))
    x <- list()
    expect_identical(make_labels_quarters(x),
                     character())
    x <- list(as.Date(c(NA, NA)))
    expect_identical(make_labels_quarters(x),
                     NA_character_)
})


## make_labels_months -------------------------------------------------------

test_that("'make_labels_months' gives correct answer with valid inputs", {
    x <- list(as.Date(c("2020-04-01", "2020-05-01")),
              as.Date(c(NA, "2020-01-01")),
              as.Date(c("2020-10-01", NA)),
              as.Date(c(NA, NA)))
    expect_identical(make_labels_months(x),
                     c("2020 Apr", "<2020 Jan", "2020 Oct+", NA))
    x <- list()
    expect_identical(make_labels_months(x),
                     character())
    x <- list(as.Date(c(NA, NA)))
    expect_identical(make_labels_months(x),
                     NA_character_)
})


## make_labels_dateranges -------------------------------------------------------

test_that("'make_labels_dateranges' gives correct answer with valid inputs", {
    x <- list(as.Date(c("2020-04-01", "2020-05-01")),
              as.Date(c(NA, "2020-01-01")),
              as.Date(c("2020-10-01", NA)),
              as.Date(c(NA, NA)))
    expect_identical(make_labels_dateranges(x),
                     c("[2020-04-01, 2020-04-30]",
                       "(-Inf, 2019-12-31]",
                       "[2020-10-01, Inf)",
                       NA))
    x <- list()
    expect_identical(make_labels_dateranges(x),
                     character())
    x <- list(as.Date(c(NA, NA)))
    expect_identical(make_labels_dateranges(x),
                     NA_character_)
})


## make_labels_datepoints -------------------------------------------------------

test_that("'make_labels_datepoints' gives correct answer with valid inputs", {
    x <- as.Date(c("2020-04-01",
                   "2020-05-01",
                   NA,
                   "2020-01-01"))
    expect_identical(make_labels_datepoints(x),
                     as.character(x))
    x <- as.Date(character())
    expect_identical(make_labels_datepoints(x),
                     character())
    x <- as.Date(NA)
    expect_identical(make_labels_datepoints(x),
                     NA_character_)
})




