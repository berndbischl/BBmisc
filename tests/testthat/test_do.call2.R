context("do.call2")

test_that("do.call2", {
  f = function(...) list(...)
  expect_equal(do.call2("f", a=1, b=2), list(a=1, b=2))
  expect_equal(do.call2("f", .args=list(a=1, b=2)), list(a=1, b=2))
  expect_equal(do.call2("f", a=1, .args=list(b=2)), list(a=1, b=2))

  oos = iris
  expect_equal(do.call2("f", oos), list(oos))

  f = function(x, data) data[[x]]
  do.call2("f", "Species", data=iris)
  do.call2("f", data=iris, .args=list(x = "Species"))
})
