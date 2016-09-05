source("../../bat_estimation.R")

context("Helper Functions")
test_that("get.column.index returns the correct number", {
    test.df <- data.frame(a = numeric(), b = character(), c = logical())
    
    expect_equal(get.column.index(test.df,"a"), 1)
    expect_equal(get.column.index(test.df,"b"), 2)
    expect_equal(get.column.index(test.df,"c"), 3)
})

test_that("get.column.index returns an empty vector when the column is not found", {
    test.df <- data.frame(a = numeric(), b = character(), c = logical())
    
    expect_equal(get.column.index(test.df,"aa"), numeric())
})

test_that("get.line.shapes returns all the existing line shapes", {
    stops.data <- data.frame(route_short_name = c("1","1","2","2","2","A"), shape_id = c(10,11,15,15,17,20))
    
    expect_equal(get.line.shapes(stops.data,"1"),data.frame(shape_id=c(10,11)))
    expect_equal(get.line.shapes(stops.data,"2"),data.frame(shape_id=c(15,17)))
    expect_equal(get.line.shapes(stops.data,"A"),data.frame(shape_id=c(20)))
})

test_that("get.line.shapes returns an empty vector for inexisting line shapes", {
    stops.data <- data.frame(route_short_name = c("1","1","2","2","2","A"), shape_id = c(10,11,15,15,17,20))
    
    expect_equal(get.line.shapes(stops.data,"3"),data.frame(shape_id=numeric()))
})

test_that("get.prev.var returns the previous value when it exists and belongs to the current trip", {
    bus.gps.data <- data.frame(var = c(1:10), trip.num = rep(1,10))
    
    expect_equal(get.prev.var(bus.gps.data,2,1,1,"var"),1)
    expect_equal(get.prev.var(bus.gps.data,3,1,1,"var"),2)
    expect_equal(get.prev.var(bus.gps.data,4,1,1,"var"),3)
})

test_that("get.prev.var returns -1 when the current row is the first in the data frame", {
    bus.gps.data <- data.frame(var = c(1:10), trip.num = rep(1,10))
    
    expect_equal(get.prev.var(bus.gps.data,1,1,1,"var"),-1)
})

test_that("get.prev.var returns -1 when the current row is the first in the trip", {
    bus.gps.data <- data.frame(var = c(1:10), trip.num = rep(1,10))
    
    expect_equal(get.prev.var(bus.gps.data,2,2,1,"var"),-1)
    expect_equal(get.prev.var(bus.gps.data,5,5,1,"var"),-1)
})

test_that("get.prev.var returns -1 when the previous row belongs to an earlier trip", {
    bus.gps.data <- data.frame(var = c(1:10), trip.num = c(1,rep(NA,9)))
    
    expect_equal(get.prev.var(bus.gps.data,2,1,2,"var"),-1)
})