library(methods)
library(devtools)
library(testthat)


load_all("skel")


f = function(x) {
  bb = parallelGetExported("big")
  x + bb[1,1]
}

g = function(big) {
  parallelExport("big")
  parallelMap(f, 1:3)
}

w = matrix(1, 100, 5)

parallelStart(mode="snowfall", cpus=2)

y = g(w)
print(y)

parallelStop()

print(gc())

