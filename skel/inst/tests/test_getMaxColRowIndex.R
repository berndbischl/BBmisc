context("getMaxColIndex")

test_that("getMaxColIndex", {
  a = matrix(1:6, nrow=2)
  expect_equal(getMaxColIndex(a), c(3L, 3L))
  a = matrix(6:1, nrow=2)
  expect_equal(getMaxColIndex(a), c(1L, 1L))
  a = rbind(c(1, 999), c(-1, -5))
  expect_equal(getMaxColIndex(a), c(2L, 1L))
  a = matrix(rnorm(50*10), nrow=50)
  expect_equal(getMaxColIndex(a), apply(a, 1, which.max))
})

test_that("getMaxRowIndex", {
  a = matrix(1:6, nrow=2)
  expect_equal(getMaxRowIndex(a), c(2L, 2L, 2L))
  a = matrix(6:1, nrow=2)
  expect_equal(getMaxRowIndex(a), c(1L, 1L, 1L))
  a = rbind(c(1, 999), c(-1, -5))
  expect_equal(getMaxRowIndex(a), c(1L, 1L))
  a = matrix(rnorm(50*10), nrow=50)
  expect_equal(getMaxRowIndex(a), apply(a, 2, which.max))
})

test_that("normal", {
  expect_equal(getMaxColIndex(diag(10)), 1:10)
  n = 100
  perm = sample(n)
  D = diag(n)
  expect_equal(getMaxColIndex(D[perm, ]), (1:n)[perm])
})

test_that("NA values", {
  n = 300
  m = matrix(runif(n), ncol=3)
  mm = m
  mm[, 2] = NA
  expect_equal(getMaxColIndex(mm), rep(NA_integer_, n/3))
})

#FIXME i dont get this test from olaf?
# why should we disregard Inf values?

# test_that("infinite values", {
#   n = 300
#   m = matrix(runif(n), ncol=3)
#   m[, 2] = -1
#   mm = m
#   mm[, 2] = Inf
#   expect_equal(getMaxColIndex(m), getMaxColIndex(mm))
# })

test_that("max.col oddity", {
  expect_equal(getMaxColIndex(cbind(1:10, 2:11, -Inf)), rep(2, 10))
  expect_equal(getMaxColIndex(cbind(-1e9 * 1:10, 1:10, 2:11)), rep(3, 10))
})

test_that("ties", {
  a = matrix(c(1, 1, 2, 2), nrow=2, byrow=TRUE)
  expect_equal(getMaxColIndex(a, ties.method="first"), c(1L, 1L))
  expect_equal(getMaxColIndex(a, ties.method="last"), c(2L, 2L))
  a = matrix(c(2, 1, 2, 2, 2, 1), nrow=2, byrow=TRUE)
  expect_equal(getMaxColIndex(a, ties.method="first"), c(1L, 1L))
  expect_equal(getMaxColIndex(a, ties.method="last"), c(3L, 2L))
  a = matrix(c(1, 1, 2, 2), nrow=2, byrow=TRUE)
  expect_equal(getMaxRowIndex(a, ties.method="first"), c(2L, 2L))
  expect_equal(getMaxRowIndex(a, ties.method="last"), c(2L, 2L))
  a = matrix(c(2, 1, 2, 2, 2, 1), nrow=2, byrow=TRUE)
  expect_equal(getMaxRowIndex(a, ties.method="first"), c(1L, 2L, 1L))
  expect_equal(getMaxRowIndex(a, ties.method="last"), c(2L, 2L, 1L))
})


