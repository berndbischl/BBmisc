
context("suppressAll")

test_that("suppressAll", {
  expect_equal(suppressAll(123), 123)
  expect_output({print(123);0}, "123")
  expect_output(suppressAll({print(123);0}), NA)
  
  expect_warning({warning(123);0}, "123")
  expect_warning(suppressAll({warning(123);0}), NA)

  expect_message({message(123);0}, "123")
  expect_message(suppressAll({message(123);0}), NA)
})

