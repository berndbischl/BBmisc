#' Chunk elements of vectors into blocks of nearly equal size.
#'
#' In case of shuffling and vectors that cannot be chunked evenly,
#' it is chosen randomly which levels / chunks will receive 1 element less.
#' If you do not shuffle, always the last chunks will receive 1 element less.
#'
#' @param x [\code{vector} | \code{list}]\cr
#'   Vector or list to chunk.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Requested number of elements in each chunk.
#'   Cannot be used in combination with \code{n.chunks}.
#'   If \code{x} cannot be evenly chunked, some chunks will have 1 element less. 
#' @param n.chunks [\code{integer(1)}]\cr
#'   Requested number of chunks.
#'   Can not be used in combination with \code{chunks.size}
#' @param shuffle [\code{logical(1)}]\cr
#'   Shuffle \code{x}?
#'   Default is \code{FALSE}.
#' @return [unnamed \code{list}] of chunks.
#' @export
#' @examples
#' xs <- 1:10
#' chunk(xs, chunk.size=3)
#' chunk(xs, n.chunks=2)
#' chunk(xs, n.chunks=2, shuffle=TRUE)
chunk = function(x, chunk.size, n.chunks, shuffle=FALSE) {
  checkArg(x, "vector")
  nx = length(x)
  if (!xor(missing(chunk.size), missing(n.chunks)))
    stop("You must provide either chunk.size (x)or n.chunks")
  if (missing(n.chunks)) {
    chunk.size = convertInteger(chunk.size)
    checkArg(chunk.size, "integer", len=1L, lower=1L, na.ok=FALSE)
    n.chunks = nx %/% chunk.size + (nx %% chunk.size > 0L)
  } else {
    n.chunks = convertInteger(n.chunks)
    checkArg(n.chunks, "integer", len=1L, lower=1L, na.ok=FALSE)
  }
  checkArg(shuffle, "logical", len=1L, na.ok=FALSE)

  n.chunks = min(n.chunks, nx)
  if (shuffle) {
    # take care that we randomly choose levels which potentially have less elems
    ch = sample(c(seq(0L, (nx %/% n.chunks) * n.chunks - 1L) %% n.chunks + 1L,
      sample(n.chunks, nx %% n.chunks)))
  } else {
    ch = sort(seq(0L, nx - 1L) %% n.chunks) + 1
  }
  unname(split(x, ch))
}
