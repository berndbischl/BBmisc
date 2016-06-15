
context("suppressAll")

test_that("suppressAll", {
  expect_equal(suppressAll(123), 123)
  expect_output({print(123);0}, "123")
  expect_silent(suppressAll({print(123);0}))

  expect_warning(warning(123), "123")
  expect_silent(suppressAll({warning(123);0}))

  expect_message(message(123), "123")
  expect_silent(suppressAll(message(123)))
})
