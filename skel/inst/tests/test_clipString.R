context("clipString")

test_that("clipString", {
  expect_equal(clipString("abcdef", 7), "abcdef")
  expect_equal(clipString("abcdef", 6), "abcdef")
  expect_equal(clipString("abcdef", 5), "ab...")
})