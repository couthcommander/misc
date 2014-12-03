library(misc)
context("round print")
test_that("rp rounds and prints n digits", {
  expect_error(rp(pi, -5))
  expect_equal(rp(1.500, 3), "1.500")
  expect_equal(rp(1.500001, 3), "1.500")
  expect_equal(rp(1.499999, 3), "1.500")
})
