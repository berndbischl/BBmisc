# blocked/chunked apply
chunkApply = function(X, FUN, FUN.VALUE, n.chunks, chunk.size, ...) {
  if(! xor(missing(n.chunks), missing(chunk.size)))
    stop("You must provide either n.chunks or chunk.size")
  FUN = match.fun(FUN)

  n = length(X)
  if(missing(chunk.size))
    chunk.size = n %/% n.chunks + (n %% n.chunks > 0L)
  else
    n.chunks = n %/% chunk.size + (n %% chunk.size > 0L)

  chunks = seq(0L, n - 1L) %/% chunk.size + 1L

  fun = function(i, ...) FUN(X[chunks == i], ...)
  if(missing(FUN.VALUE))
    lapply(seq_len(n.chunks), fun, ...)
  else
    vapply(seq_len(n.chunks), fun, FUN.VALUE, ...)
}
