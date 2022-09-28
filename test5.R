library(microbenchmark)

load_all()


n = 50000
p = 50

x = matrix(rnorm(n * p), nrow = n)
w = rep(1, p)

mb = microbenchmark(
  getMaxIndexOfCols(x),
  # getMaxIndexOfRows(x, weights = w),
  colMaxs(x)
)
print(mb)




