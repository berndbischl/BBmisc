
context("asMatrix")

test_that("asMatrix", {
  # empty
  expect_equal(
    asMatrixCols(list()),
    matrix(0, nrow = 0L, ncol = 0L)
  )
  expect_equal(
    asMatrixRows(list()),
    matrix(0, nrow = 0L, ncol = 0L)
  )

  # normal
  expect_equal(
    asMatrixCols(list(c(1, 2), c(3, 3), c(4, 4))),
    matrix(c(1, 2, 3, 3, 4, 4), nrow = 2, ncol = 3, byrow = FALSE)
  )
  expect_equal(
    asMatrixRows(list(c(1, 2), c(3, 3), c(4, 4))),
    matrix(c(1, 2, 3, 3, 4, 4), nrow = 3, ncol = 2, byrow = TRUE)
  )

  # names
  expect_equal(
    asMatrixCols(list(a = c(1, 2), b = c(3, 3), c = c(4, 4))),
    setColNames(matrix(c(1, 2, 3, 3, 4, 4), nrow = 2, ncol = 3, byrow = FALSE), c("a", "b", "c"))
  )
  expect_equal(
    asMatrixRows(list(a = c(1, 2), b = c(3, 3), c = c(4, 4))),
    setRowNames(matrix(c(1, 2, 3, 3, 4, 4), nrow = 3, ncol = 2, byrow = TRUE), c("a", "b", "c"))
  )
  expect_equal(
    asMatrixRows(list(a = c(x = 1, y = 2), b = c(3, 3), c = c(4, 4))),
    setColNames(setRowNames(matrix(c(1, 2, 3, 3, 4, 4), nrow = 3, ncol = 2, byrow = TRUE),
        c("a", "b", "c")), c("x", "y"))
  )
})

