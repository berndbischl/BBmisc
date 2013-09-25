context("isScalarValue")

test_that("isScalarValue", {
  expect_true(isScalarValue(1))
  expect_true(isScalarValue(1L))
  expect_true(isScalarValue("a"))
  expect_true(isScalarValue(factor("a")))
  expect_true(isScalarValue(as.complex(1)))
  expect_true(isScalarValue(NA))
  
  expect_false(isScalarValue(NULL))
  expect_false(isScalarValue(iris))
  expect_false(isScalarValue(1:2))
  expect_false(isScalarValue(list(1)))
  
  expect_true(isScalarValue(NULL, null.ok=TRUE))
  expect_false(isScalarValue(NULL, na.ok=FALSE))
})