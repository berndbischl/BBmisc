context("findFirst / findLast")

test_that("findFirst / findLast", {
  x = c(FALSE, TRUE, FALSE, TRUE)
  expect_equal(findFirst(x), 2L)
  expect_equal(findLast(x), 4L)

  x = c(FALSE, TRUE, NA, TRUE)
  expect_equal(findFirst(x), 2L)
  expect_equal(findLast(x), 4L)
  expect_equal(findFirst(x, na.omit=FALSE), 2L)
  expect_equal(findLast(x, na.omit=FALSE), 4L)

  x = c(NA, TRUE, NA, TRUE, NA)
  expect_equal(findFirst(x, na.omit=FALSE), NA_integer_)
  expect_equal(findLast(x, na.omit=FALSE), NA_integer_)

  x = logical(0L)
  expect_equal(findFirst(x, na.omit=FALSE), integer(0L))
  expect_equal(findLast(x, na.omit=FALSE), integer(0L))
  expect_equal(findFirst(x, na.omit=TRUE), integer(0L))
  expect_equal(findLast(x, na.omit=TRUE), integer(0L))

  x = c(NA, NA)
  expect_equal(findFirst(x, na.omit=FALSE), NA_integer_)
  expect_equal(findLast(x, na.omit=FALSE), NA_integer_)
  expect_equal(findFirst(x, na.omit=TRUE), integer(0L))
  expect_equal(findLast(x, na.omit=TRUE), integer(0L))
})
