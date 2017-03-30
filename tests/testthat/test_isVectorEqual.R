context("isVectorEqual")

test_that("isVectorEqual", {
  expect_equal(isVectorEqual(1:3, 1:3), c(TRUE, TRUE, TRUE))
  expect_equal(isVectorEqual(1:3, c(1,2,1)), c(TRUE, TRUE, FALSE))
  expect_equal(isVectorEqual(c(1,2,NA), c(1,2,NA)), c(TRUE, TRUE, TRUE))
  expect_equal(isVectorEqual(NA, NA), TRUE)
  expect_error(isVectorEqual(1, 1:2), "Vecors are not of the same length")
})
