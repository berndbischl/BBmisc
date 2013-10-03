context("any.missing")

test_that("any.missing", {
          library(testthat)
  setNA = function(x, i = sample(length(x), 1)) {
    if (is.list(x))
      x[i] = list(NULL)
    else
      is.na(x) = i
    x
  }
  xb = logical(5)
  xi = integer(5)
  xd = double(5)
  xc = complex(5)
  xs = character(5)
  xl = vector("list", 5)
  xl[] = 1

  expect_false(any.missing(xb))
  expect_false(any.missing(xi))
  expect_false(any.missing(xd))
  expect_false(any.missing(xc))
  expect_false(any.missing(xs))
  expect_false(any.missing(xl))
  expect_true(any.missing(setNA(xb)))
  expect_true(any.missing(setNA(xi)))
  expect_true(any.missing(setNA(xd)))
  expect_true(any.missing(setNA(xc)))
  expect_true(any.missing(setNA(xs)))
  expect_true(any.missing(setNA(xl)))

  expect_false(any.missing(NULL))
  expect_false(any.missing(iris))
  expect_error(any.missing(new.env()))
})


