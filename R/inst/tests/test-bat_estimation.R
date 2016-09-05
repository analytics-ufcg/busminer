source("../../bat_estimation.R")

context("Helper Functions")
test_that("get.column.index returns the correct number", {
    test.df <- data.frame(a = numeric(), b = character(), c = logical())
    
    expect_that(get.column.index(test.df,"a"), equals(1))
    expect_that(get.column.index(test.df,"b"), equals(2))
    expect_that(get.column.index(test.df,"c"), equals(3))
})