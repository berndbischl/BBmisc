context("ensureLength")

test_that("ensureLength", {
  expect_equal(ensureLength("a", 2L), c("a", "a"))
  expect_equal(ensureLength(1, 1), c(1))

  expect_equal(ensureLength(iris, 1), list(iris))
  expect_equal(ensureLength(iris, 2), list(iris, iris))
  expect_equal(ensureLength(iris, 2, names = c("a", "b")), list(a = iris, b = iris))
})

