library(devtools)
load_all()

# x = 1:3
# w = c(100, 1, 1)
# j = getMaxIndex(x, weights = w)
# print(j)


x = matrix(c(1,1,2,1,1,3), ncol = 3)
print(x)
j = getMaxIndexOfRows(x, weights = c(2.5, 1, 1))
print(j)

