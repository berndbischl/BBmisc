context("is.subset (and isSuperset)")

test_that("is.subset/is.superset", {
  x = 1:10
  y = 1:11
  expect_true(is.subset(x, y))
  expect_false(is.subset(y, x))
  expect_true(is.subset(x, y, strict = TRUE))

  x = y
  expect_true(is.subset(x, y))
  expect_false(is.subset(x, y, strict = TRUE))
  expect_true(is.subset(y, x))
  expect_false(is.subset(y, x, strict = TRUE))
})
