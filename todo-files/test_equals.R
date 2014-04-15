context("equals")

test_that("equals", {
  y = c(-1L,5L,Inf, NA)
  x = c(-1, 5, Inf, NA)
  expect_equal(x %equals% y, c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(x %equals% y, y %equals% x)
  x = c("abc", "def", NA, "ghi", NA)
  y = c("abc", "DEF", "ghi", "ghi", NA)
  expect_equal(x %equals% y, c(TRUE, FALSE, FALSE, TRUE, TRUE))
})
