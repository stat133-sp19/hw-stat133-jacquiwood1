library(testthat)
#context("Test checkers")

test_that("check_prob with valid prob", {
  
  expect_true(check_prob(0.5))
  expect_true(check_prob(0))
  expect_true(check_prob(10/10))
})


test_that("check_prob fails with invalid prob", {
  
  expect_error(check_prob(-0.5))
  expect_error(check_prob("one hundred"))
  expect_error(check_prob(50/5))
})


test_that("check_trials with valid trials", {
  
  expect_true(check_trials(1))
  expect_true(check_trials(0))
  expect_true(check_trials(10000))
})


test_that("check_trials fails with invalid trials", {
  
  expect_error(check_trials(-1))
  expect_error(check_trials("100"))
  expect_error(check_trials(0.5))
})

test_that("check_success with valid success", {
  
  expect_true(check_success(4,5))
  expect_true(check_success(0,100))
  expect_true(check_success(0,0))
})


test_that("check_success fails with invalid success", {
  
  expect_error(check_success(-1,5))
  expect_error(check_success(1:5,4))
  expect_error(check_success(-1:2,2))
})