context("clearFunctionEnv")

test_that("clearFunctionEnv", {
  getF = function() {
    x = 1L
    function() x
  }

  f = getF()
  expect_equal(f(), 1L)
  f = clearFunctionEnv(f)
  expect_error(f())
})
