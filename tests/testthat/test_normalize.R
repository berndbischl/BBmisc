context("normalize")

test_that("normalize", {
  # vector
  x = runif(20)
  y = normalize(x, method = "range")
  expect_equal(range(y), c(0, 1))
  y = normalize(x, method = "range", range = c(-4, 2))
  expect_equal(range(y), c(-4, 2))
  y = normalize(x, method = "center")
  expect_equal(mean(y), 0)
  y = normalize(x, method = "standardize")
  expect_equal(mean(y), 0)
  expect_equal(sd(y), 1)

  # matrix
  x = matrix(runif(100), nrow = 5)
  y = normalize(x)

  # data.frame
  y = normalize(iris, method = "range", range = c(3, 4))
  for (i in 1:4)
    expect_equal(range(y[, i]), c(3, 4))
  y[, 5L] = iris$Specis
})


