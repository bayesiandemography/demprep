
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
                     factor(c("0-4", "10+", "0-4"),
                            levels = c("0-4", "5-9", "10+")))
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

## ## format_age_births --------------------------------------------------

## test_that("format_age_births gives correct answers with valid inputs", {
##     expect_identical(format_age_births(date = c("2015-01-01",
##                                                      "2025-01-01",
##                                                      "2029-12-31"),
##                                             dob = "2000-01-01"),
##                      factor(c("15-19", "25-29", "25-29"),
##                             levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")))
##     expect_identical(format_age_births(date = c("2015-01-01",
##                                                      "2025-01-01",
##                                                      "2029-12-31"),
##                                             dob = "2000-01-01",
##                                             break_min = 10,
##                                             break_max = 55),
##                      factor(c("15-19", "25-29", "25-29"),
##                             levels = c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")))
##     expect_identical(format_age_births(date = c("2015-01-01",
##                                                      "2025-01-01",
##                                                      "2029-12-31"),
##                                             dob = "2000-01-01",
##                                             break_min = 15,
##                                             break_max = 50,
##                                             width = 1),
##                      factor(c("15", "25", "29"),
##                             levels = 15:49))
##     expect_identical(format_age_births(date = c("2015-01-01",
##                                                      "2025-01-01",
##                                                      "2049-12-31"),
##                                             dob = "2000-01-01",
##                                             break_min = 20,
##                                             break_max = 40,
##                                             recode_up = TRUE,
##                                             recode_down = TRUE),
##                      factor(c("20-24", "25-29", "35-39"),
##                             levels = c("20-24", "25-29", "30-34", "35-39")))
##     expect_identical(format_age_births(date = c(NA,
##                                                      NA,
##                                                      "2049-12-31"),
##                                             dob = NA_character_,
##                                             break_min = 20,
##                                             break_max = 40,
##                                             recode_up = TRUE,
##                                             recode_down = TRUE),
##                      factor(rep(NA_character_, 3),
##                             levels = c("20-24", "25-29", "30-34", "35-39")))
## })

## test_that("format_age_births throws correct errors with invalid inputs", {
##     expect_error(format_age_births(date = c("2015-01-01",
##                                                  "2025-01-01",
##                                                  "2029-12-31"),
##                                         dob = "2000-01-01",
##                                         width = 3),
##                  "difference between 'break_max' \\[50\\] and 'break_min' \\[15\\] not divisible by 'width' \\[3\\]")
##     expect_error(format_age_births(date = c("2015-01-01",
##                                                  "2025-01-01",
##                                                  "2029-12-31"),
##                                         dob = "2000-01-01",
##                                         break_min = 20),
##                  paste("'date' of \"2015-01-01\" and 'dob' of \"2000-01-01\" imply age of 15,",
##                        "but 'break_min' is 20 and 'recode_up' is FALSE"))
##     expect_error(format_age_births(date = c("2045-01-01",
##                                                  "2025-01-01",
##                                                  "2029-12-31"),
##                                         dob = "2000-01-01",
##                                         break_max = 45),
##                  paste("'date' of \"2045-01-01\" and 'dob' of \"2000-01-01\" imply age of 45,",
##                        "but 'break_max' is 45 and 'recode_down' is FALSE"))
## })

## test_that("format_age_births throws correct errors with invalid inputs", {
##     expect_error(format_age_custom(date = c("2001-06-01",
##                                                        "2015-01-01",
##                                                        "2016-12-31"),
##                                               dob = "2000-01-01",
##                                           breaks = c(5, 10, 30)),
##                  "'date' of \"2001-06-01\" and 'dob' of \"2000-01-01\" imply age of 1, but minimum value for 'breaks' is 5")
##     expect_error(format_age_custom(date = c("2001-06-01",
##                                                        "2015-01-01",
##                                                        "2026-12-31"),
##                                               dob = "2000-01-01",
##                                           breaks = c(0, 10, 20),
##                                           open_last = FALSE),
##                  "'date' of \"2026-12-31\" and 'dob' of \"2000-01-01\" imply age of 26, but 'open_last' is FALSE and maximum value for 'breaks' is 20")
## })


## ## format_age_custom ---------------------------------------------------

## test_that("format_age_custom gives correct answers with valid inputs", {
##     expect_identical(format_age_custom(date = c("2003-01-01",
##                                                        "2025-01-01",
##                                                        "2039-12-31"),
##                                               dob = "2000-01-01",
##                                               breaks = c(0, 10, 30)),
##                      factor(c("0-9", "10-29", "30+"),
##                             levels = c("0-9", "10-29", "30+")))
##     expect_identical(format_age_custom(date = c("2000-06-01",
##                                                        "2015-01-01",
##                                                        "2016-12-31"),
##                                               dob = "2000-01-01",
##                                               breaks = c(0, 1, 30),
##                                               open_last = FALSE),
##                      factor(c("0", "1-29", "1-29"),
##                             levels = c("0", "1-29")))
##     expect_identical(format_age_custom(date = c("2005-06-01",
##                                                        "2015-01-01",
##                                                        "2016-12-31"),
##                                               dob = "2000-01-01",
##                                               breaks = c(5, 10, 30),
##                                               open_last = FALSE),
##                      factor(c("5-9", "10-29", "10-29"),
##                             levels = c("5-9", "10-29")))
##     expect_identical(format_age_custom(date = c("2000-03-11",
##                                                        NA),
##                                               dob = c(NA,
##                                                       "2000-01-01"),
##                                               breaks = c(0, 10, 20),
##                                               open_last = FALSE),
##                      factor(c(NA, NA),
##                             levels = c("0-9", "10-19")))
##     expect_identical(format_age_custom(date = c("2000-03-11",
##                                                        NA),
##                                               dob = c(NA,
##                                                       "2000-01-01"),
##                                               breaks = integer(),
##                                               open_last = FALSE),
##                      factor(c(NA, NA)))
## })

## test_that("format_age_custom gives correct error with invalid inputs", {
##     expect_error(format_age_custom(date = c("2000-03-11",
##                                                        "2000-03-12"),
##                                                dob = c(NA,
##                                                        "2000-01-01"),
##                                                breaks = integer(),
##                                                open_last = FALSE),
##                  "'breaks' has length 0")
## })


## ## format_age_quarter ---------------------------------------------------

## test_that("format_age_quarter gives correct answers with valid inputs", {
##     expect_identical(format_age_quarter(date = c("2000-01-01",
##                                                         "2000-05-11",
##                                                         "2001-02-28"),
##                                                dob = "2000-01-01"),
##                      factor(c("0q", "1q", "4q"),
##                             levels = c(paste0(0:399, "q"), "400q+")))
##     expect_identical(format_age_quarter(date = c("2000-01-01",
##                                                         "2000-05-11",
##                                                         "2001-04-28"),
##                                                dob = "2000-01-01",
##                                                break_max = 5L),
##                      factor(c("0q", "1q", "5q+"),
##                             levels = c(paste0(0:4, "q"), "5q+")))
##     expect_identical(format_age_quarter(date = c("2000-01-01",
##                                                         "2000-05-11",
##                                                         NA,
##                                                         "2001-04-28"),
##                                                dob = "2000-01-01",
##                                                break_max = 6,
##                                                open_last = FALSE),
##                      factor(c("0q", "1q", NA, "5q"),
##                             levels = c("0q", "1q", "2q", "3q", "4q", "5q")))
##     expect_identical(format_age_quarter(date = c("2000-03-11",
##                                                         NA),
##                                                dob = c(NA,
##                                                        "2000-01-01"),
##                                                break_max = NULL,
##                                                open_last = FALSE),
##                      factor(c(NA, NA)))
##     expect_identical(format_age_quarter(date = c("2000-03-11",
##                                                         NA),
##                                                dob = c(NA,
##                                                        "2000-01-01"),
##                                                break_max = 3,
##                                                open_last = FALSE),
##                      factor(c(NA, NA), levels = c("0q", "1q", "2q")))
## })

## ## format_age_month ---------------------------------------------------

## test_that("format_age_month gives correct answers with valid inputs", {
##     expect_identical(format_age_month(date = c("2000-01-01",
##                                                       "2000-03-11",
##                                                       "2000-02-29"),
##                                              dob = "2000-01-01"),
##                      factor(c("0m", "2m", "1m"),
##                             levels = c(paste0(0:1199, "m"), "1200m+")))
##     expect_identical(format_age_month(date = c("2000-01-01",
##                                                       "2000-03-11",
##                                                       "2000-02-29"),
##                                              dob = "2000-01-01",
##                                              break_max = 3),
##                      factor(c("0m", "2m", "1m"),
##                             levels = c("0m", "1m", "2m", "3m+")))
##     expect_identical(format_age_month(date = c("2000-01-01",
##                                                       "2000-03-11",
##                                                       NA,
##                                                       "2000-02-29"),
##                                              dob = "2000-01-01",
##                                              break_max = 3,
##                                              open_last = FALSE),
##                      factor(c("0m", "2m", NA, "1m"),
##                             levels = c("0m", "1m", "2m")))
##     expect_identical(format_age_month(date = c("2000-03-11",
##                                                       NA),
##                                              dob = c(NA,
##                                                      "2000-01-01"),
##                                              break_max = 3,
##                                              open_last = FALSE),
##                      factor(c(NA, NA), levels = c("0m", "1m", "2m")))
##     expect_identical(format_age_month(date = c("2000-03-11",
##                                                       NA),
##                                              dob = c(NA,
##                                                      "2000-01-01"),
##                                              break_max = NULL,
##                                              open_last = FALSE),
##                      factor(c(NA, NA)))
## })
