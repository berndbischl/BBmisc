#' Chunk elements of vectors into blocks of nearly equal size.
#'
#' @param x [\code{vector} | \code{list}]\cr
#'  Vector or list to chunk into blocks of nearly equal size.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Preferred number of jobs in each chunk.
#'   Can not be used in combination with \code{n.chunks}
#' @param n.chunks [\code{integer(1)}]\cr
#'   Preferred number of chunks.
#'   Can not be used in combination with \code{chunks.size}
#' @param shuffle [\code{logical(1)}]\cr
#'   Shuffle the vector or list?
#'   Default is \code{FALSE}.
#' @return [unnamed \code{list}] of chunks.
#' @export
#' @examples
#' xs <- 1:10
#' chunk(xs, chunk.size=3)
#' chunk(xs, n.chunks=2)
#' chunk(xs, n.chunks=2, shuffle=TRUE)
chunk = function(x, chunk.size, n.chunks, shuffle=FALSE) {
  if (!xor(missing(chunk.size), missing(n.chunks)))
    stop("You must provide either chunk.size (x)or n.chunks")

  n = length(x)
  if (!missing(n.chunks)) {
    n.chunks = convertInteger(n.chunks)
    checkArg(n.chunks, "integer", len=1L, lower=1L, na.ok=FALSE)
    ch = sort(seq(0L, n - 1L) %% n.chunks + 1L)
  } else { # !missing(chunk.size)
    chunk.size = convertInteger(chunk.size)
    checkArg(chunk.size, "integer", len=1L, lower=1L, na.ok=FALSE)
    n.chunks = n %/% chunk.size + (n %% chunk.size > 0L)
    ch = head(rep(seq_len(n.chunks), each=chunk.size), n)
  }

  if(shuffle)
    x = sample(x)
  unname(split(x, ch))
}
