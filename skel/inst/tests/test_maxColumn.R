context("maxColumn")

test_that("normal", {
  expect_equal(maxColumn(diag(10)), 1:10)

  n <- 100
  perm <- sample(n)
  D <- diag(n)
  expect_equal(maxColumn(D[perm, ]), (1:n)[perm])
})

test_that("NA values", {
  n <- 300
  m <- matrix(runif(n), ncol=3)
  mm <- m
  mm[, 2] <- NA
  expect_equal(maxColumn(mm), rep(NA_integer_, n/3))
})

test_that("infinite values", {
  n <- 300
  m <- matrix(runif(n), ncol=3)
  m[, 2] <- -1
  mm <- m
  mm[, 2] <- Inf
  expect_equal(maxColumn(m), maxColumn(mm))
})

test_that("max.col oddity", {
  expect_equal(maxColumn(cbind(1:10, 2:11, -Inf)), rep(2, 10))
  expect_equal(maxColumn(cbind(-1e9 * 1:10, 1:10, 2:11)), rep(3, 10))
})
