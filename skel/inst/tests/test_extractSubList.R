context("extractSubList")

test_that("extractSubList", {
  xs = list(
    a = list(x=1, y="foo", z=matrix(1,1,1)),
    b = list(x=2L, y="bar", z=matrix(2,2,2))
  )
  expect_equal(extractSubList(xs, "x"), c(a=1, b=2))
  expect_equal(extractSubList(xs, "y"), c(a="foo", b="bar"))
  expect_equal(extractSubList(xs, "z"), list(a=matrix(1,1,1), b=matrix(2,2,2)))

  expect_equal(extractSubList(xs, "x", use.names=FALSE), c(1, 2))
  expect_equal(extractSubList(xs, "y", use.names=FALSE), c("foo", "bar"))
  expect_equal(extractSubList(xs, "z", use.names=FALSE), list(matrix(1,1,1), matrix(2,2,2)))
  
  expect_equal(extractSubList(list(), "x"), list())
  
  expect_equal(extractSubList(list(), "x", element.value=numeric(1)), numeric(0))
  expect_equal(extractSubList(list(), "y", element.value=character(1)), character(0))
  
  expect_equal(extractSubList(xs, "x", element.value=numeric(1)), c(a=1, b=2))
  expect_equal(extractSubList(xs, "y", element.value=character(1)), c(a="foo", b="bar"))
  
  xs = list(
    list(x=1, y="foo", z=matrix(1,1,1)),
    list(x=2L, y="bar", z=matrix(2,2,2))
  )
  expect_equal(extractSubList(xs, "y", use.names=TRUE), c("foo", "bar"))
  expect_equal(extractSubList(xs, "y", use.names=FALSE), c("foo", "bar"))
})


