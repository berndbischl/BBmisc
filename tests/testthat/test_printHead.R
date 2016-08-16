context("printHead")

test_that("printHead", {
  expect_output(printHead(1:10), "...")
  expect_output(printHead(as.list(1:10)), "...")
  expect_output(printHead(iris), "...")
  expect_output(printHead(matrix(runif(20), ncol = 2L)), "...")
})

