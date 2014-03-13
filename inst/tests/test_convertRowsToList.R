context("convertRowsToList")

test_that("convertRowsToList", {
  expect_equal(
    convertRowsToList(matrix(1:4, 2, byrow=TRUE), as.vector = TRUE),
    list(c(1, 2), c(3, 4))
  )
  expect_equal(
    convertRowsToList(matrix(1:4, 2, byrow=TRUE), as.vector = FALSE),
    list(list(1, 2), list(3, 4))
  )
  expect_equal(
    convertRowsToList(setColNames(matrix(1:4, 2, byrow=TRUE), c("a", "b")), use.names=TRUE, as.vector = FALSE),
    list(list(a=1, b=2), list(a=3, b=4))
  )
  expect_equal(
    convertRowsToList(setColNames(matrix(1:4, 2, byrow=TRUE), c("a", "b")), use.names=FALSE, as.vector = FALSE),
    list(list(1, 2), list(3, 4))
  )
  levs = c("a", "b")
  expect_equal(
    convertRowsToList(data.frame(a=1:2, b=factor(c("a", "b"))), use.names=FALSE, factors.as.char=TRUE),
    list(list(1, "a"), list(2, "b"))
  )
  expect_equal(
    convertRowsToList(data.frame(a=1:2, b=factor(c("a", "b"))), use.names=TRUE, factors.as.char=FALSE),
    list(list(a=1, b=factor("a", levels=levs)), list(a=2, b=factor("b", levels=levs)))
  )
})

test_that("convertColsToList", {
  expect_equal(
    convertColsToList(matrix(1:4, 2, byrow = FALSE), as.vector = TRUE),
    list(c(1, 2), c(3, 4))
  )
  expect_equal(
    convertColsToList(matrix(1:4, 2, byrow = FALSE), as.vector = FALSE),
    list(list(1, 2), list(3, 4))
  )
  expect_equal(
    convertColsToList(setRowNames(matrix(1:4, 2, byrow = FALSE), c("a", "b")), use.names=TRUE, as.vector = FALSE),
    list(list(a=1, b=2), list(a=3, b=4))
  )
})

