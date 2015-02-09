context("capitalizeStrings")

test_that("capitalizeStrings", {
  testString = c("the taIl", "wags The dOg", "That Looks fuNny!")
  
  expect_equal(capitalizeStrings(testString)
               , c("The taIl", "Wags The dOg", "That Looks fuNny!"))
  expect_equal(capitalizeStrings(testString, all.words = TRUE)
               , c("The TaIl", "Wags The DOg", "That Looks FuNny!"))
  expect_equal(capitalizeStrings(testString, all.words = TRUE, lower.back = TRUE)
               , c("The Tail", "Wags The Dog", "That Looks Funny!"))
  expect_equal(capitalizeStrings(testString, lower.back = TRUE)
               , c("The tail", "Wags the dog", "That looks funny!"))
})

