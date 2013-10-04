context("checkRule")

test_that("checkRule", {
  library(testthat)

  xb = logical(10); xb[5] = NA
  xi = integer(10); xi[5] = NA
  xr = double(10); xr[5] = NA
  xc = complex(10); xc[5] = NA
  xl = vector("list", 10); xl[[5]] = list(NULL)
  xm = matrix(1:9, 3); xm[2, 3] = NA
  xd = data.frame(a=1:5, b=1:5); xd$b[3] = NA

  # test type and missingness
  expect_true(checkRule(xb, "b"))
  expect_false(checkRule(xb, "B"))
  expect_true(checkRule(xi, "i"))
  expect_false(checkRule(xi, "I"))
  expect_true(checkRule(xr, "r"))
  expect_false(checkRule(xr, "R"))
  expect_true(checkRule(xc, "c"))
  expect_false(checkRule(xc, "C"))
  expect_true(checkRule(xl, "l"))
  expect_false(checkRule(xl, "L"))
  expect_true(checkRule(xm, "m"))
  expect_false(checkRule(xm, "M"))
  expect_true(checkRule(xd, "d"))

  expect_false(checkRule(xd, "b"))
  expect_false(checkRule(xd, "i"))
  expect_false(checkRule(xd, "r"))
  expect_false(checkRule(xd, "c"))
  expect_false(checkRule(xd, "l"))
  expect_false(checkRule(xd, "m"))
  expect_false(checkRule(xm, "d"))

  # test length
  expect_true(checkRule(xb, "b+"))
  expect_true(checkRule(xb, "b10"))
  expect_true(checkRule(logical(1), "b+"))
  expect_true(checkRule(logical(1), "b?"))
  expect_true(checkRule(logical(1), "b1"))
  expect_false(checkRule(xb, "b?"))
  expect_false(checkRule(xb, "b5"))

  # test other types
  expect_true(checkRule(function() 1, "*"))
  expect_true(checkRule(function() 1, "f"))
  expect_false(checkRule(function() 1, "b"))
  expect_true(checkRule(new.env(), "e"))
  expect_false(checkRule(new.env(), "b"))

  expect_true(checkRule(xb, "a+"))
  expect_true(checkRule(xi, "a+"))
  expect_true(checkRule(xr, "a+"))
  expect_true(checkRule(xm, "a+"))
  expect_false(checkRule(xl, "a+"))
  expect_false(checkRule(new.env(), "a+"))

  expect_true(checkRule(NULL, "0?"))
  expect_false(checkRule(xi, "0"))
})
