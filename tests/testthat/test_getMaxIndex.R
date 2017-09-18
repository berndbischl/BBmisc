context("getMaxIndex")

test_that("getMaxIndex", {
  expect_equal(getMaxIndex(c(1, 9)), 2L)
  expect_equal(getMaxIndex(c(9, 1)), 1L)
  expect_equal(getMaxIndex(c(-9, -1)), 2L)
  expect_equal(getMaxIndex(c(-9, 1)), 2L)
  expect_equal(getMaxIndex(c(1, Inf, 9)), 2L)
  expect_equal(getMaxIndex(c(1, NA, 9)), NA_integer_)
  expect_equal(getMaxIndex(c(1, NaN, 9)), NA_integer_)
  expect_equal(getMaxIndex(c(1, NA, 9), na.rm = TRUE), 3L)
  expect_equal(getMaxIndex(c(1, NaN, 9), na.rm = TRUE), 3L)
  expect_equal(getMaxIndex(numeric(0)), integer(0))
  expect_equal(getMaxIndex(c()), integer(0))
  expect_equal(getMaxIndex(c(NA, NaN), na.rm = TRUE), integer(0))
})

test_that("getMinIndex", {
  expect_equal(getMinIndex(c(1, 9)), 1L)
  expect_equal(getMinIndex(c(9, 1)), 2L)
  expect_equal(getMinIndex(c(-9, -1)), 1L)
  expect_equal(getMinIndex(c(-9, 1)), 1L)
  expect_equal(getMinIndex(c(1, Inf, 9)), 1L)
  expect_equal(getMinIndex(c(1, NA, 9)), NA_integer_)
  expect_equal(getMinIndex(c(1, NaN, 9)), NA_integer_)
  expect_equal(getMinIndex(c(1, NA, 9), na.rm = TRUE), 1L)
  expect_equal(getMinIndex(c(1, NaN, 9), na.rm = TRUE), 1L)
  expect_equal(getMinIndex(numeric(0)), integer(0))
  expect_equal(getMinIndex(c()), integer(0))
  expect_equal(getMinIndex(c(NA, NaN), na.rm = TRUE), integer(0))
})

test_that("ties", {
  expect_equal(getMaxIndex(c(1, 9, 9), ties.method = "first"), 2L)
  expect_equal(getMaxIndex(c(1, 9, 9), ties.method = "last"), 3L)
  expect_equal(getMaxIndex(3, ties.method = "first"), 1L)
  expect_equal(getMaxIndex(3, ties.method = "last"), 1L)
  expect_equal(getMaxIndex(c(9, 1, 9, 9), ties.method = "first"), 1L)
  expect_equal(getMaxIndex(c(9, 1, 9, 9), ties.method = "last"), 4L)
})

test_that("getBestIndex", {
  # here we simply compare with the output of the already tested delegates
  expect_equal(getBestIndex(c(9, 1, 3)), getMinIndex(c(9, 1, 3)))
  expect_equal(getBestIndex(c(9, 1, 3), minimize = FALSE), getMaxIndex(c(9, 1, 3)))
  expect_equal(getBestIndex(c(NA), na.rm = TRUE), getMinIndex(c(NA), na.rm = TRUE))
  expect_equal(getBestIndex(c(1, 1, 9, 5), ties.method = "last"), getMinIndex(c(1, 1, 9, 5), ties.method = "last"))
})

test_that("getMaxIndex with weights", {
  expect_equal(getMaxIndex(c(1, 9), c(1, 1)), 2L)
  expect_equal(getMaxIndex(c(1, 9), c(1, -1)), 1L)
  expect_equal(getMaxIndex(c(1, 9), c(100, 1)), 1L)

  expect_equal(getMinIndex(c(1, 9), c(1, 1)), 1L)
  expect_equal(getMinIndex(c(1, 9), c(1, -1)), 2L)
  expect_equal(getMinIndex(c(1, 9), c(100, 1)), 2L)

  is.na(getMaxIndex(c(1, NA, 2), c(1, 1, 1), na.rm = FALSE))
  expect_equal(getMaxIndex(c(1, NA, 2), c(1, 1, 1), na.rm = TRUE), 3L)
  expect_equal(getMaxIndex(c(1, NA, 2), c(5, 1, 1), na.rm = TRUE), 1L)
  expect_equal(getMaxIndex(c(1, NA, 2), c(NA, 1, 1), na.rm = TRUE), 3L)
})

