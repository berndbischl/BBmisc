context("first / last")

test_that("first / last", {
  x = c(FALSE, TRUE, FALSE, TRUE)
  expect_equal(first(x), 2L)
  expect_equal(last(x), 4L)

  x = c(FALSE, TRUE, NA, TRUE)
  expect_equal(first(x), 2L)
  expect_equal(last(x), 4L)
  expect_equal(first(x, na.omit=FALSE), 2L)
  expect_equal(last(x, na.omit=FALSE), 4L)

  x = c(NA, TRUE, NA, TRUE, NA)
  expect_equal(first(x, na.omit=FALSE), NA_integer_)
  expect_equal(last(x, na.omit=FALSE), NA_integer_)
  
  x = logical(0L)
  expect_equal(first(x, na.omit=FALSE), integer(0L))
  expect_equal(last(x, na.omit=FALSE), integer(0L))
  expect_equal(first(x, na.omit=TRUE), integer(0L))
  expect_equal(last(x, na.omit=TRUE), integer(0L))
  
  x = c(NA, NA)
  expect_equal(first(x, na.omit=FALSE), NA_integer_)
  expect_equal(last(x, na.omit=FALSE), NA_integer_)
  expect_equal(first(x, na.omit=TRUE), integer(0L))
  expect_equal(last(x, na.omit=TRUE), integer(0L))
})
