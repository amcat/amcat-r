#' Test version string reading 

context("AmCAT-R version tests")


test_that("Version can be read", {
  v = read_version("3.4.1")
  expect_equal(v, c(major=3, minor=4, patch=1))
  v = read_version("2.06.12dev0")
  expect_equal(v, c(major=2, minor=6, patch=12))
  v = read_version("3.42dev0")
  expect_equal(v, c(major=0, minor=0, patch=0))
})

test_that("has_version works", {
  expect_true(has_version("3.4.1", "3.4.1"))
  expect_true(has_version("3.4.1", "3.4.0"))
  expect_true(has_version("3.4.1", "2.7.12"))

  expect_false(has_version("xxx", "2.7.12"))
  expect_false(has_version(NULL, "2.7.12"))
  expect_false(has_version("3.4.2", "3.4.11"))
})