context("list")

test_that("insert", {
  xs1 = list(a=1, b=2)
  expect_equal(insert(xs1, list(a=99, c=5)), list(a=99, b=2, c=5))
  expect_equal(insert(xs1, list(a=list(99), c=5)), list(a=list(99), b=2, c=5))
})

test_that("listToShortString", {
  expect_equal(listToShortString(list()), "")
  expect_equal(listToShortString(list(a=1)), "a=1")
  expect_equal(listToShortString(list(a=1:2)), "a=1 2")
  expect_equal(listToShortString(list(a=1:20)), "a=1 2 3 4 5 6 7 8 9 10 ...")
  expect_equal(listToShortString(list(a=1, 2, b=3)), "a=1, <unnamed>=2, b=3")
  expect_equal(listToShortString(list(a=1, 2, b=data.frame())), "a=1, <unnamed>=2, b=<data.frame>")
  expect_equal(listToShortString(list(a=identity, b=new.env())), "a=<function>, b=<environment>")
})