library(devtools)
library(microbenchmark)
load_all()

x = computeMode3(c("1","2","3"), ties.method = "last")
print(str(x))

# computeMode2 = function (x, ties.method = "random", na.rm = TRUE) {
#   tab = data.table::as.data.table(x)[, ".N", by = list(x)]
#   if(na.rm) tab = na.omit(tab)
#   mod = tab$x[which.max(tab$N)]
#   if (!is.factor(x))
#     mode(mod) = mode(x)
#   if (length(mod) > 1L)
#     switch(ties.method, first = mod[1L], random = sample(mod, 1L), last = mod[length(mod)])
#   else mod
# }

# v = as.integer(c(-69, -69, 39, 77, 50, 72, 100))
# m = computeMode3(v)
# print(m)

# # set.seed(1)

# # kseq = seq(10, 100000, length.out = 5)

# # for (k in kseq) {
# #   v = sample(100, k, replace = TRUE)
# #   m1 = computeMode(v, ties = "first")
# #   # print(m1)
# #   m2 = computeMode3(v)
# #   # print(m2)
# #   # print(m1 == m2)
# #   if (m1 != m2)
# #     stop("sdasd")
# #   m = microbenchmark(computeMode(v), computeMode2(v), computeMode3(v))
# #   print(k)
# #   print(summary(m, unit = "ms")[, "median"])
# # }

# # logical, int, real, complex, string, factor

