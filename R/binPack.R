binPack = function(x, capacity) {
  checkArg(x, "numeric", min.len=1L, na.ok=FALSE)
  checkArg(capacity, "numeric", len=1L, na.ok=FALSE)
  too.big = head(which(x > capacity), 1L)
  if (length(too.big))
    stopf("Capacity not sufficient. Item %i (x=%f) does not fit", too.big, x[too.big])
  ord = order(x, decreasing=TRUE)
  packs = list(numeric(0L))
  sums = 0

  for(j in ord) {
    x.cur = x[j]
    pos = head(which(x.cur + sums < capacity), 1L)
    if (length(pos)) {
      packs[[pos]] = c(packs[[pos]], j)
      sums[pos] = sums[pos] + x.cur
    } else {
      packs = c(packs, list(j))
      sums = c(sums, x.cur)
    }
  }
  return(list(packs = packs, sums = sums))
}

binPack(runif(10), 1)
binPack(runif(10000), 1)
