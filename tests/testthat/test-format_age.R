
context("format_age")

## format_age_year --------------------------------------------------

test_that("format_age_year gives correct answers with alternative finite values for 'break_max'", {
    expect_identical(format_age_year(x = c("0", "10", "4")),
                     factor(c(0, 10, 4), levels = c(0:99, "100+")))
    expect_identical(format_age_year(x = c("0", "10+", "4"),
                                      break_max = 10),
                     factor(c(0, "10+", 4), levels = c(0:9, "10+")))
    expect_identical(format_age_year(x = c("1", "2+", "100"),
                                      break_max = 2),
                     factor(c(1, "2+", "2+"), levels = c(0:1, "2+")))
})

test_that("format_age_year gives correct answers with no value supplied for 'break_max'", {
    expect_identical(format_age_year(x = c("0", "10+", "4"),
                                     break_max = NULL),
                     factor(c(0, "10+", 4), levels = c(0:9, "10+")))
    expect_identical(format_age_year(x = c("0", "10", "4"),
                                     break_max = NULL,
                                     open_last = FALSE),
                     factor(c(0, "10", 4), levels = as.character(0:10)))
})

test_that("format_age_year gives correct answers when 'open_last' is FALSE", {
    expect_identical(format_age_year(x = c("0", "10", "4"),
                                     open_last = FALSE),
                     factor(c(0, 10, 4), levels = 0:99))
})

test_that("format_age_year gives correct answers when 'x' has NA", {
    expect_identical(format_age_year(x = c("0", "10", NA)),
                     factor(c(0, "10", NA),
                            levels = c(0:99, "100+", NA),
                            exclude = NULL))
})

test_that("format_age_year gives correct answers when 'x' only has NAs", {
    expect_identical(format_age_year(x = as.character(c(NA, NA))),
                     factor(rep(NA_character_, 2),
                            levels = c(0:99, "100+", NA),
                            exclude = NULL))
    expect_identical(format_age_year(x = as.character(c(NA, NA)),
                                     break_max = NULL),
                     factor(rep(NA_character_, 2),
                            levels = NA,
                            exclude = NULL))
})

test_that("format_age_year gives correct answers when 'x' has length 0", {
    expect_identical(format_age_year(x = character()),
                     factor(character(), levels = c(0:99, "100+")))
})

test_that("format_age_year gives expected errors when given invalid inputs", {
    expect_error(format_age_year(x = c("0", "10", "5+"),
                                 break_max = 10),
                 "lower limit of open interval \"5\\+\" is less than 'break_max' \\[10\\]")
})


## format_age_multi --------------------------------------------------

test_that("format_age_multi gives correct answers with valid inputs", {
    expect_identical(format_age_multi(x = c("0", "10-12", "0-4")),
                     factor(c("0-4", "10-14", "0-4"),
                            levels = c(paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"),
                                       "100+")))
    expect_identical(format_age_multi(x = c("1", "11+", "4"),
                                      break_max = 10),
                     factor(c("0-4", "10+", "0-4"),
                            levels = c("0-4", "5-9", "10+")))
    expect_identical(format_age_multi(x = c("0-8", "100+", "5"),
                                      width = 10,
                                      break_max = 10),
                     factor(c("0-9", "10+", "0-9"), levels = c("0-9", "10+")))
    expect_identical(format_age_multi(x = c("0", "11", "0-4"),
                                      break_max = NULL),
                     factor(c("0-4", "10-14", "0-4"),
                            levels = c("0-4", "5-9", "10-14", "15+")))
    expect_identical(format_age_multi(x = c("1-4", "14", "0"),
                                      break_max = NULL,
                                      open_last = FALSE),
                     factor(c("0-4", "10-14", "0-4"),
                            levels = c("0-4", "5-9", "10-14")))
    expect_identical(format_age_multi(x = c("1-4", "14", "0", NA),
                                      break_max = NULL,
                                      open_last = FALSE),
                     factor(c("0-4", "10-14", "0-4", NA),
                            levels = c("0-4", "5-9", "10-14", NA),
                            exclude = NULL))
    expect_identical(format_age_multi(x = rep(NA_character_, 3),
                                      break_max = NULL,
                                      open_last = FALSE),
                     factor(rep(NA_character_, 3),
                            levels = NA,
                            exclude = NULL))
    expect_identical(format_age_multi(x = rep(NA_character_, 3),
                                      break_max = 10,
                                      open_last = FALSE),
                     factor(rep(NA_character_, 3),
                            levels = c("0-4", "5-9", NA),
                            exclude = NULL))
})


## format_age_lifetab --------------------------------------------------

test_that("format_age_lifetab gives correct answers with valid inputs", {
    expect_identical(format_age_lifetab(x = c("1", "10-14", "3")),
                     factor(c("1-4", "10-14", "1-4"),
                            levels = c("0", "1-4", paste(seq(5, 95, 5), seq(9, 99, 5), sep = "-"),
                                       "100+")))
    expect_identical(format_age_lifetab(x = c("0", "100", "1", NA),
                                        break_max = 10),
                     factor(c("0", "10+", "1-4", NA),
                            levels = c("0", "1-4", "5-9", "10+", NA),
                            exclude = NULL))
    expect_identical(format_age_lifetab(x = rep(NA_character_, 3),
                                        break_max = 10),
                     factor(rep(NA_character_, 3),
                            levels = c("0", "1-4", "5-9", "10+", NA),
                            exclude = NULL))
    expect_identical(format_age_lifetab(x = rep(NA_character_, 3),
                                        break_max = NULL),
                     factor(rep(NA_character_, 3),
                            levels = NA,
                            exclude = NULL))
})

## format_age_births --------------------------------------------------

test_that("format_age_births gives correct answers with valid inputs", {
    expect_identical(format_age_births(x = c("18-19", "25-29", NA, "29")),
                     factor(c("15-19", "25-29", NA, "25-29"),
                            levels = c("15-19", "20-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49", NA),
                            exclude = NULL))
    expect_identical(format_age_births(x = c("18-19", "25-29", "29"),
                                       break_min = 10,
                                       break_max = 55),
                     factor(c("15-19", "25-29", "25-29"),
                            levels = c("10-14", "15-19", "20-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49", "50-54")))
    expect_identical(format_age_births(x = c("15", "25", "29"),
                                       break_min = 15,
                                       break_max = 50,
                                       width = 1),
                     factor(c("15", "25", "29"),
                            levels = 15:49))
    expect_identical(format_age_births(x = c("15-19", "29", "46"),
                                       break_min = 20,
                                       break_max = 40,
                                       recode_up = TRUE,
                                       recode_down = TRUE),
                     factor(c("20-24", "25-29", "35-39"),
                            levels = c("20-24", "25-29", "30-34", "35-39")))
    expect_identical(format_age_births(x = c(NA, NA, NA),
                                       break_min = 20,
                                       break_max = 40,
                                       recode_up = TRUE,
                                       recode_down = TRUE),
                     factor(rep(NA_character_, 3),
                            levels = c("20-24", "25-29", "30-34", "35-39", NA),
                            exclude = NULL))
})

test_that("format_age_births throws correct errors with invalid inputs", {
    expect_error(format_age_births(x = "20+"),
                 "'x' contains open age group \\[\"20\\+\"]")
    expect_error(format_age_births(x = "22",
                                   width = 3),
                 "difference between 'break_max' \\[50\\] and 'break_min' \\[15\\] not divisible by 'width' \\[3\\]")
    expect_error(format_age_births(x = "15",
                                   break_min = 20),
                 "age group \"15\" less than 'break_min' \\[20\\] and 'recode_up' is FALSE")
    expect_error(format_age_births(x = c("20", "50-54"),
                                   break_max = 45),
                 paste("age group \"50-54\" greater than 'break_max' \\[45\\] and 'recode_down' is FALSE"))
})


## format_age_custom ---------------------------------------------------

test_that("format_age_custom gives correct answers with valid inputs", {
    expect_identical(format_age_custom(x = c(NA, "0", "23-24", "100+"),
                                       breaks = c(0, 10, 30)),
                     factor(c(NA, "0-9", "10-29", "30+"),
                            levels = c("0-9", "10-29", "30+", NA),
                            exclude = NULL))
    expect_identical(format_age_custom(x = c("0", "1", "5-29"),
                                       breaks = c(0, 1, 30),
                                       open_last = FALSE),
                     factor(c("0", "1-29", "1-29"),
                            levels = c("0", "1-29")))
    expect_identical(format_age_custom(x = c("8", "11", NA, "20-29"),
                                       breaks = c(5, 10, 30),
                                       open_last = FALSE),
                     factor(c("5-9", "10-29", NA, "10-29"),
                            levels = c("5-9", "10-29", NA),
                            exclude = NULL))
    expect_identical(format_age_custom(x = c(NA, NA),
                                       breaks = c(0, 10, 20),
                                       open_last = FALSE),
                     factor(c(NA, NA),
                            levels = c("0-9", "10-19", NA),
                            exclude = NULL))
})

test_that("format_age_custom throws correct errors with invalid inputs", {
    expect_error(format_age_custom(x = c(NA, NA),
                                   breaks = integer(),
                                   open_last = FALSE),
                 "'breaks' has length 0")
    expect_error(format_age_custom(x = "1-4",
                                   breaks = c(5, 10, 30)),
                 "age group \"1-4\" is below minimum value for 'breaks' \\[5\\]")
    expect_error(format_age_custom(x = "15-29",
                                   breaks = c(0, 10, 20),
                                   open_last = FALSE),
                 "age group \"15-29\" is above maximum value for 'breaks' \\[20\\]")
    expect_error(format_age_custom(x = "1",
                                   breaks = integer(),
                                   open_last = FALSE),
                 "'breaks' has length 0")
})


## format_age_quarter ---------------------------------------------------

test_that("format_age_quarter gives correct answers with valid inputs", {
    expect_identical(format_age_quarter(x = c("0", "1", "4")),
                     factor(c("0", "1", "4"),
                            levels = c(0:399, "400+")))
    expect_identical(format_age_quarter(c("0", "1", "100"),
                                        break_max = 5L),
                     factor(c("0", "1", "5+"),
                            levels = c(0:4, "5+")))
    expect_identical(format_age_quarter(x = c("0", "1", NA, "5"),
                                        break_max = 6,
                                        open_last = FALSE),
                     factor(c("0", "1", NA, "5"),
                            levels = c("0", "1", "2", "3", "4", "5", NA),
                            exclude = NULL))
    expect_identical(format_age_quarter(x = c(NA, NA),
                                        break_max = NULL,
                                        open_last = FALSE),
                     factor(c(NA, NA),
                            levels = NA_character_,
                            exclude = NULL))
    expect_identical(format_age_quarter(x = c(NA, NA),
                                        break_max = 3,
                                        open_last = FALSE),
                     factor(c(NA, NA),
                            levels = c("0", "1", "2", NA),
                            exclude = NULL))
})

## format_age_month ---------------------------------------------------

test_that("format_age_month gives correct answers with valid inputs", {
    expect_identical(format_age_month(x = c("0", "1", "4")),
                     factor(c("0", "1", "4"),
                            levels = c(0:1199, "1200+")))
    expect_identical(format_age_month(c("0", "1", "100"),
                                      break_max = 5L),
                     factor(c("0", "1", "5+"),
                            levels = c(0:4, "5+")))
    expect_identical(format_age_month(x = c("0", "1", NA, "5"),
                                      break_max = 6,
                                      open_last = FALSE),
                     factor(c("0", "1", NA, "5"),
                            levels = c("0", "1", "2", "3", "4", "5", NA),
                            exclude = NULL))
    expect_identical(format_age_month(x = c(NA, NA),
                                      break_max = NULL,
                                      open_last = FALSE),
                     factor(c(NA, NA),
                            levels = NA_character_,
                            exclude = NULL))
    expect_identical(format_age_month(x = c(NA, NA),
                                      break_max = 3,
                                      open_last = FALSE),
                     factor(c(NA, NA),
                            levels = c("0", "1", "2", NA),
                            exclude = NULL))
})

