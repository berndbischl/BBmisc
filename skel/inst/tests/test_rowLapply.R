context("rowLapply")

test_that("rowLapply", {
  df = data.frame(a = 1:10, b = 10:1)
  expect_true(all(rowLapply(df, length, unlist=TRUE) == 2))
  expect_true(all(rowLapply(df, sum, unlist=TRUE) == 11))
  expect_true(all(unlist(rowLapply(df, Negate(is.list), unlist=TRUE))))
  expect_true(all(unlist(rowLapply(df, is.list))))
})
