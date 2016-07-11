computeMode2 = function (x, ties.method = "random", na.rm = TRUE)
{
  assertAtomicVector(x)
  assertChoice(ties.method, c("first", "random", "last"))
  assertFlag(na.rm)
  tab = data.table::as.data.table(x)[, ".N", by = list(x)]
  if(na.rm) tab = na.omit(tab)
  mod = tab$x[which.max(tab$N)]
  if (!is.factor(x))
    mode(mod) = mode(x)
  if (length(mod) > 1L)
    switch(ties.method, first = mod[1L], random = sample(mod, 1L), last = mod[length(mod)])
  else mod
}
