context("namedLapply")

test_that("namedLapply", {
  l = list(a = "a", 5, c = NULL, d = NA)
  l = namedLapply(l, paste, sep = "")
  print(l)
  expect_equal(l, list(a = "aa", "5", c = "c", d = "NAd"))
})