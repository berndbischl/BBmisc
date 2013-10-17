\name{chunk}
\alias{chunk}
\title{Chunk elements of vectors into blocks of nearly equal size.}
\usage{
  chunk(x, chunk.size, n.chunks, shuffle = FALSE)
}
\arguments{
  \item{x}{[\code{vector} | \code{list}]\cr Vector or list
  to chunk into blocks of nearly equal size.}

  \item{chunk.size}{[\code{integer(1)}]\cr Preferred number
  of jobs in each chunk.  Can not be used in combination
  with \code{n.chunks}}

  \item{n.chunks}{[\code{integer(1)}]\cr Preferred number
  of chunks.  Can not be used in combination with
  \code{chunks.size}}

  \item{shuffle}{[\code{logical(1)}]\cr Shuffle the vector
  or list?  Default is \code{FALSE}.}
}
\value{
  [unnamed \code{list}] of chunks.
}
\description{
  Chunk elements of vectors into blocks of nearly equal
  size.
}
\examples{
xs <- 1:10
chunk(xs, chunk.size=3)
chunk(xs, n.chunks=2)
chunk(xs, n.chunks=2, shuffle=TRUE)
}

