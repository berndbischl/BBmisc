context("getCurrentTimeSecs")

test_that("getCurrentTimeSecs", {
  x = getCurrentTimeSecs()
  expect_true(is.integer(x) && length(x) == 1 && !is.na(x))
})