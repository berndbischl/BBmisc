context("chunk")

test_that("chunk", {
  x = 1:10
  ch = chunk(x, chunk.size = 3)
  expect_equal(ch, list(1:3, 4:6, 7:9, 10))

  ch = chunk(1:9, n.chunks=4)
  expect_equal(length(ch), 4)
  
  x = letters[1:10]
  ch = chunk(x, n.chunks = 2)
  expect_equal(ch, list(letters[1:5], letters[6:10]))
  
  x = letters[1:10]
  expect_error(chunk(x, chunk.size = 1, n.chunks = 3))
  expect_error(chunk(x, chunk.size=1:2))
  expect_error(chunk(x, n.chunks=list()))


  x = as.list(letters[1:10])
  ch = chunk(x, chunk.size = 5)
  expect_equal(ch, list(as.list(letters[1:5]), as.list(letters[6:10])))

  x = letters
  ch = chunk(x, chunk.size = 4, shuffle=TRUE)
  expect_equal(sort(letters), sort(unlist(ch)))
  expect_true(all(sapply(ch, length) %in% c(2, 4)))
})

