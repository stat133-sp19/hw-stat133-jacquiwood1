library(testthat)
#context("Test summary measures")

test_that("aux_mean returns the correct mean", {
  
  expect_equal(aux_mean(10,0.3),3)
  expect_equal(aux_mean(20,0.6),12)
  expect_equal(aux_mean(5,0.2),1)
})


test_that("aux_variance returns the correct variance", {
  
  expect_equal(aux_variance(10,0.3),2.1)
  expect_equal(aux_variance(20,0.6),4.8)
  expect_equal(aux_variance(5,0.2),0.8)
})


test_that("aux_mode returns the correct mode", {
  
  expect_equal(aux_mode(10,0.3),3)
  expect_equal(aux_mode(20,0.6),12)
  expect_equal(aux_mode(5,0.2),1)
  expect_equal(aux_mode(5,0.2),c(3,2))
})

test_that("aux_skewness returns the correct skewness", {
  
  expect_equal(aux_skewness(20,0.6),-0.09128709)
  expect_equal(aux_skewness(5,0.2),0.6708204)
  expect_equal(aux_skewness(10,0.5),0)
})


test_that("aux_kurtosis returns the correct kurtosis", {
  
  expect_equal(aux_kurtosis(20,0.6),-0.09166667)
  expect_equal(aux_kurtosis(5,0.2),0.05)
  expect_equal(aux_kurtosis(10,0.5),-0.2)
})