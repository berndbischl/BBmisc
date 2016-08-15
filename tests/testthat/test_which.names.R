context("which.names")

test_that("which.names", {
  x = c(a = FALSE, b = TRUE, c = FALSE, d = TRUE)
  expect_equal(which.names(x), c("b", "d"))
})
