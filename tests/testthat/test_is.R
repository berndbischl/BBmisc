context("%is%")

test_that("%is%'", {
  x = 1L
  x.classes = c("classA", "classB")
  attr(x, "class") = x.classes
  expect_true(x %is% "classA")
  expect_true(x %is% "classB")
  expect_false(x %is% "classC")
  expect_error(x %is% x.classes, info = "%is% only checks for inheritance of ONE class, not multiple!")
})
