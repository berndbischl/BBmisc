\name{binPack}
\alias{binPack}
\title{Simple bin packing}
\usage{
binPack(x, capacity)
}
\arguments{
  \item{x}{[\code{numeric}]\cr Numeric vector of elements
  to group.}

  \item{capacity}{[\code{numeric(1)}]\cr Maximum capacity
  of each bin, i.e. elements will be grouped to not excess
  this limit.}
}
\value{
[\code{integer}]. Integer with values \dQuote{1} to
\dQuote{n.bins} indicating bin membership.
}
\description{
Splits numeric items in \code{x} into groups with sum less
or equal than \code{capacity}. If one element of \code{x}
exceeds \code{capacity}, an error is thrown.
}
\examples{
x = 1:10
bp = binPack(x, 11)
xs = split(x, bp)
print(xs)
print(sapply(xs, sum))
}

