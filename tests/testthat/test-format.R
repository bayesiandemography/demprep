
test_that("'reformat_age' gives correct answer with valid inputs", {
    x <- c("0 Year", "1 to 4 Years", "5 to 9 Years", "10 Years And Over")
    ans.obtained <- reformat_age(x)
    ans.expected <- c("0", "1-4", "5-9", "10+")
    expect_identical(ans.obtained, ans.expected)
    x <- c("0 yr", "1--4 yrs", "5--9 yrs", "10plus")
    ans.obtained <- reformat_age(x)
    ans.expected <- c("0", "1-4", "5-9", "10+")
    expect_identical(ans.obtained, ans.expected)
})


