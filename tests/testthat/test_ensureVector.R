context("ensureVector")

test_that("ensureVector", {
  # default is n = 1L
  expect_equal(ensureVector("a"), "a")
  expect_equal(ensureVector("a", n = 2L), c("a", "a"))
  expect_equal(ensureVector("a", n = 2L, cl = "integer"),  "a")
  expect_equal(ensureVector(1, n = 1), c(1))

  expect_equal(ensureVector(c("a", "b"), n = 10L), c("a", "b"))

  expect_equal(ensureVector(iris, n = 1L), list(iris))
  expect_equal(ensureVector(iris, n = 2L, cl = "matrix"), iris)
  expect_equal(ensureVector(iris, n = 2L, cl = "data.frame"), list(iris, iris))
  expect_equal(ensureVector(iris, n = 2L), list(iris, iris))
  expect_equal(ensureVector(iris, n = 2L, names = c("a", "b")), list(a = iris, b = iris))

  # check ensure.list argument
  expect_equal(ensureVector("a", ensure.list = TRUE), list("a"))
  expect_equal(ensureVector(3, n = 3L, ensure.list = TRUE, names = letters[1:3]), list(a = 3, b = 3, c = 3))
})

