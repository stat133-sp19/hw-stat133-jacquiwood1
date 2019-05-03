library(testthat)
#context("Test binomial")

test_that("bin_choose returns correct number of combinations", {
  
  expect_equal(bin_choose(5,2),10)
  expect_equal(bin_choose(10,10),1)
  expect_error(bin_choose(2,5))
})


test_that("bin_probability returns the correct binomial probability", {
  
  expect_equal(bin_probability(2,5,0.5),0.3125)
  expect_equal(bin_probability(10,10,1),1)
  expect_error(bin_probability(5,2,0.5))
  expect_equal(bin_probability(0:2,5,0.5),c(0.03125,0.15625,0.31250))
})


test_that("bin_distribution returns the correct distribution", {
  
  expect_equal(bin_distribution(5,0.5)$probability,c(0.03125,0.15625,0.31250,0.31250,0.15625,0.03125))
  expect_equal(bin_distribution(5,0.5)$success,c(0,1,2,3,4,5))
  expect_equal(bin_distribution(3,0.3)$probability,c(0.343,0.441,0.189,0.027))
})


test_that("bin_cumulative returns the correct cumulative distribution", {
  
  expect_equal(bin_cumulative(5,0.5)$cumulative,c(0.03125,0.18750,0.50000,0.81250,0.96875,1.00000))
  expect_equal(bin_cumulative(3,0.3)$cumulative,c(0.343,0.784,0.973,1.000))
  expect_equal(bin_cumulative(1,1)$cumulative,c(0,1))
})
