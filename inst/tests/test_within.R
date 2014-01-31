context("within")

test_that("within", {
  y = c(-1L,5L,Inf)
  expect_equal(3L %within% y, TRUE)
  expect_equal(-2L %within% y, FALSE)
  y = 5L
  expect_equal(5L %within% y, TRUE)
  expect_equal(1L %within% y, FALSE)
})
