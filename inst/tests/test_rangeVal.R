
context("rangeVal")

test_that("rangeVal", {
  expect_equal(rangeVal(c(1, 5)), 4)
  expect_equal(rangeVal(1), 0)
  expect_equal(rangeVal(1:3), 2)
})
