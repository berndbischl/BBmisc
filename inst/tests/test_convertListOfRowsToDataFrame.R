context("convertListOfRowstoDataFrame")

test_that("convertListOfRowstoDataFrame", {
  df1 = convertListOfRowsToDataFrame(list(list(x=1, y="a"), list(x=2, y="b")), strings.as.factors=FALSE)
  df2 = data.frame(x=1:2, y=c("a", "b"), stringsAsFactors=FALSE)
  expect_equal(df1, df2)

  df1 = convertListOfRowsToDataFrame(list(c(x="1", y="a"), list(x="2", y="b")), strings.as.factors=FALSE)
  df2 = data.frame(x=c("1", "2"), y=c("a", "b"), stringsAsFactors=FALSE)
  expect_equal(df1, df2)

  df1 = convertListOfRowsToDataFrame(list(c("1", "a"), list("2", "b")), strings.as.factors=FALSE, 
    col.names=c("x", "y"))
  df2 = data.frame(x=c("1", "2"), y=c("a", "b"), stringsAsFactors=FALSE)
  expect_equal(df1, df2)
})
