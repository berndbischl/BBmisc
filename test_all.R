library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(BBmisc)
}
test_dir("tests/testthat")
