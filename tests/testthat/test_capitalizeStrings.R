context("capitalizeStrings")

test_that("capitalizeStrings", {
  testString <- c("the taIl", "wags The dOg", "That Looks fuNny!")
  
  expect_equal(capitalizeStrings(testString)
               , c("The taIl", "Wags The dOg", "That Looks fuNny!"))
  expect_equal(capitalizeStrings(testString, allWords=TRUE)
               , c("The TaIl", "Wags The DOg", "That Looks FuNny!"))
  expect_equal(capitalizeStrings(testString, allWords=TRUE, lowerBack=TRUE)
               , c("The Tail", "Wags The Dog", "That Looks Funny!"))
  expect_equal(capitalizeStrings(testString, lowerBack=TRUE)
               , c("The tail", "Wags the dog", "That looks funny!"))
})

