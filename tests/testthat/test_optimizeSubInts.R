context("optimizeSubInts")

test_that("optimizeSubInts", {

  f = function(x) sin(x) * x
  z = optimizeSubInts(f, interval = c(0, 50), nsub = 200L)
  print(z)
  fopt = f(pi * 3 / 2 + 14 * pi)
  expect_true(abs(fopt -  z$objective) < 1e-1)
})

