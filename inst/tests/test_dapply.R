context("dapply")

test_that("dapply", {
  d = dapply(1:3, function(x) rep(x, 2))
  expect_is(d, "data.frame")
  expect_equal(dim(d), c(2L, 3L))
  expect_equal(colnames(d), c("Var.1", "Var.2", "Var.3"))

  d = dapply(1:2, function(x) x, col.names=c("a", "b"))
  expect_is(d, "data.frame")
  expect_equal(dim(d), c(1L, 2L))
  expect_equal(colnames(d), c("a", "b"))
})


