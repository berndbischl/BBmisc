\name{chunk}
\alias{chunk}
\title{Chunk elements of vectors into blocks of nearly equal size.}
\usage{
chunk(x, chunk.size, n.chunks, shuffle = FALSE)
}
\arguments{
  \item{x}{[\code{vector} | \code{list}]\cr Vector or list
  to chunk.}

  \item{chunk.size}{[\code{integer(1)}]\cr Requested number
  of elements in each chunk.  Cannot be used in combination
  with \code{n.chunks}.  If \code{x} cannot be evenly
  chunked, some chunks will have 1 element less.}

  \item{n.chunks}{[\code{integer(1)}]\cr Requested number
  of chunks.  Can not be used in combination with
  \code{chunks.size}}

  \item{shuffle}{[\code{logical(1)}]\cr Shuffle \code{x}?
  Default is \code{FALSE}.}
}
\value{
[unnamed \code{list}] of chunks.
}
\description{
In case of shuffling and vectors that cannot be chunked
evenly, it is chosen randomly which levels / chunks will
receive 1 element less. If you do not shuffle, always the
last chunks will receive 1 element less.
}
\examples{
xs <- 1:10
chunk(xs, chunk.size=3)
chunk(xs, n.chunks=2)
chunk(xs, n.chunks=2, shuffle=TRUE)
}

