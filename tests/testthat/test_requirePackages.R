context("requirePackages")

test_that("requirePackages", {
  expect_equal(requirePackages("base"), c(base=TRUE))
  expect_equal(requirePackages("xxx", stop = FALSE, suppress.warnings = TRUE), c(xxx=FALSE))
  expect_error(requirePackages("xxx", suppress.warnings=TRUE), "Please install the following packages: xxx")
  expect_equal(requirePackages(c("xxx", "base"), stop=FALSE, suppress.warnings=TRUE), c(xxx=FALSE, base=TRUE))
  expect_equal(requirePackages(c("base", "xxx"), stop=FALSE, suppress.warnings=TRUE), c(base=TRUE, xxx=FALSE))
  expect_error(requirePackages(c("base", "xxx"), suppress.warnings=TRUE), "Please install the following packages: xxx")
  expect_error(requirePackages(c("base", "xxx"), why="test", suppress.warnings=TRUE), "For test please install the following packages: xxx")

  skip_if_not_installed("codetools")
  # test loading vs. attaching using the codetools package
  expect_equal(requirePackages("codetools", default.method = "load"), c(codetools=TRUE))
  expect_true("codetools" %in% loadedNamespaces())
  expect_false("package:codetools" %in% search())
  expect_equal(requirePackages("!codetools", default.method = "load"), c(codetools=TRUE))
  expect_true("package:codetools" %in% search())
})

test_that("requirePackages with min.versions", {
  expect_equal(requirePackages("base", min.versions = "0.1"), c(base=TRUE))
  expect_equal(requirePackages("base", min.versions = c(base="0.1")), c(base=TRUE))

  expect_equal(requirePackages(c("base", "stats")), c(base=TRUE, stats = TRUE))
  expect_equal(requirePackages(c("base", "stats"), min.versions = c(base = "100"), stop = FALSE), c(base=FALSE, stats = TRUE))
  expect_error(requirePackages(c("base", "stats"), min.versions = c(stats = "100")), "stats >= 100")
})

