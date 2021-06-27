
## coord_lifeline -------------------------------------------------------------

test_that("'coord_lifeline' gives correct answer with valid inputs", {
    coord_lifeline <- demprep:::coord_lifeline
    ## single boundary, no upward shift
    ans_obtained <- coord_lifeline(date1 = as.Date("2020-02-15"),
                                   dob1 = as.Date("2020-01-15"))
    ans_expected <- list(x0 = as.Date(c("2020-01-15",
                                        "2020-02-01",
                                        "2020-02-01")),
                         y0 = c(0L, 17L, 17L),
                         x1 = as.Date(c("2020-02-01",
                                        "2020-02-01",
                                        "2020-02-15")),
                         y1 = c(17L, 17L, 31L))
    expect_identical(ans_obtained, ans_expected)
    ## two boundaries, one upward shift
    ans_obtained <- coord_lifeline(date1 = as.Date("2020-03-15"),
                                   dob1 = as.Date("2020-01-15"))
    ans_expected <- list(x0 = as.Date(c("2020-01-15",
                                        "2020-02-01",
                                        "2020-02-01",
                                        "2020-03-01",
                                        "2020-03-01")),
                         y0 = c(0L,
                                17L,
                                17L,
                                46L,
                                48L),
                         x1 = as.Date(c("2020-02-01",
                                        "2020-02-01",
                                        "2020-03-01",
                                        "2020-03-01",
                                        "2020-03-15")),
                         y1 = c(17L,
                                17L,
                                46L,
                                48L,
                                62L))
    expect_identical(ans_obtained, ans_expected)
    ## start on first day of month
    ans_obtained <- coord_lifeline(date1 = as.Date("2010-03-15"),
                                   dob1 = as.Date("2010-01-01"))
    ans_expected <- list(x0 = as.Date(c("2010-01-01",
                                        "2010-02-01",
                                        "2010-02-01",
                                        "2010-03-01",
                                        "2010-03-01")),
                         y0 = c(0L,
                                31L,
                                31L,
                                59L,
                                62L),
                         x1 = as.Date(c("2010-02-01",
                                        "2010-02-01",
                                        "2010-03-01",
                                        "2010-03-01",
                                        "2010-03-15")),
                         y1 = c(31L,
                                31L,
                                59L,
                                62L,
                                76L))
    expect_identical(ans_obtained, ans_expected)
    ## start on last day of month
    ans_obtained <- coord_lifeline(date1 = as.Date("2010-03-15"),
                                   dob1 = as.Date("2010-01-31"))
    ans_expected <- list(x0 = as.Date(c("2010-01-31",
                                        "2010-02-01",
                                        "2010-02-01",
                                        "2010-03-01",
                                        "2010-03-01")),
                         y0 = c(0L,
                                1L,
                                1L,
                                29L,
                                32L),
                         x1 = as.Date(c("2010-02-01",
                                        "2010-02-01",
                                        "2010-03-01",
                                        "2010-03-01",
                                        "2010-03-15")),
                         y1 = c(1L,
                                1L,
                                29L,
                                32L,
                                46L))
    expect_identical(ans_obtained, ans_expected)
})
