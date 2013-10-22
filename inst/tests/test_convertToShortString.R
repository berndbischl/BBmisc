context("convertToShortString")

test_that("convertToShortString", {
  expect_equal(convertToShortString(1L), "1")
  expect_equal(convertToShortString(1.0), "1")
  expect_equal(convertToShortString(1.23), "1.23")
  expect_equal(convertToShortString(numeric(0)), "numeric(0)")
  expect_equal(convertToShortString(factor(c())), "factor(0)")
  expect_equal(convertToShortString(iris), "<data.frame>")
  expect_equal(convertToShortString(list(a=1, 45)), "a=1, <unnamed>=45")
  expect_equal(convertToShortString(list(a=1, b=list(x=3))), "a=1, b=<list>")
  expect_equal(convertToShortString(list(a=1, b=iris)), "a=1, b=<data.frame>")
})


