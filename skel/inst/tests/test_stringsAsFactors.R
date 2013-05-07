context("stringsAsFactors")

test_that("stringsAsFactors", {
  d1 = data.frame(a=1:2, b=rep("b", 2), stringsAsFactors=FALSE)
  d2 = data.frame(a=1:2, b=rep("b", 2), stringsAsFactors=TRUE)
  expect_equal(stringsAsFactors(d1), d2)
  expect_equal(stringsAsFactors(d2), d2)
}) 




