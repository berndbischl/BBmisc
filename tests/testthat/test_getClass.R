context("getClass")

test_that("getClass", {
  expect_equal(getClass(iris), "data.frame")
  expect_equal(getClass(1), "numeric")
  expect_equal(getClass(NULL), "NULL")
  x = makeS3Obj(c("C1", "C2"), foo = 2)
  expect_equal(getClass(x), "C1")
})


