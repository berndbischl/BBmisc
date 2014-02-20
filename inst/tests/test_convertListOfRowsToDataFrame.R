context("convertListOfRowstoDataFrame")

test_that("convertListOfRowstoDataFrame", {
  df1 = convertListOfRowsToDataFrame(list(list(x=1, y="a"), list(x=2, y="b")), strings.as.factors=FALSE)
  df2 = data.frame(x=1:2, y=c("a", "b"), stringsAsFactors=FALSE)
  expect_equal(df1, df2)

  df1 = convertListOfRowsToDataFrame(list(c(x="1", y="a"), list(x="2", y="b")), strings.as.factors=FALSE)
  df2 = data.frame(x=c("1", "2"), y=c("a", "b"), stringsAsFactors=FALSE)
  expect_equal(df1, df2)

  df1 = convertListOfRowsToDataFrame(list(list(a = 1, b = 1), list(b = 12)))
  df2 = convertListOfRowsToDataFrame(list(c(a = 1, b = 1), c(b = 12)))
  expect_equal(df1, df2)
})
