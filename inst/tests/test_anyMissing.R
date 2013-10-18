context("anyMissing")

test_that("anyMissing", {
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

  expect_false(anyMissing(xb))
  expect_false(anyMissing(xi))
  expect_false(anyMissing(xd))
  expect_false(anyMissing(xc))
  expect_false(anyMissing(xs))
  expect_false(anyMissing(xl))
  expect_true(anyMissing(setNA(xb)))
  expect_true(anyMissing(setNA(xi)))
  expect_true(anyMissing(setNA(xd)))
  expect_true(anyMissing(setNA(xc)))
  expect_true(anyMissing(setNA(xs)))
  expect_true(anyMissing(setNA(xl)))

  expect_false(anyMissing(NULL))
  expect_false(anyMissing(iris))
  expect_error(anyMissing(new.env()))
})


