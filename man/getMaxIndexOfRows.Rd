\name{getMaxIndexOfRows}
\alias{getMaxIndexOfCols}
\alias{getMaxIndexOfRows}
\alias{getMinIndexOfCols}
\alias{getMinIndexOfRows}
\title{Find row- or columnwise the index of the maximal / minimal element in a matrix.}
\usage{
getMaxIndexOfRows(x, ties.method = "random", na.rm = FALSE)

getMinIndexOfRows(x, ties.method = "random", na.rm = FALSE)

getMaxIndexOfCols(x, ties.method = "random", na.rm = FALSE)

getMinIndexOfCols(x, ties.method = "random", na.rm = FALSE)
}
\arguments{
  \item{x}{[\code{matrix(n,m)}] \cr Numerical input
  matrix.}

  \item{ties.method}{[\code{character(1)}]\cr How should
  ties be handled?  Possible are: \dQuote{random},
  \dQuote{first}, \dQuote{last}.  Default is
  \dQuote{random}.}

  \item{na.rm}{[\code{logical(1)}]\cr If \code{FALSE}, NA
  is returned if an NA is encountered in \code{x}.  If
  \code{TRUE}, NAs are disregarded.  Default is
  \code{FALSE}}
}
\value{
[\code{integer(n)}].
}
\description{
\code{getMaxIndexOfRows} returns the index of the maximal
element of each row. \code{getMinIndexOfRows} returns the
index of the minimal element of each row.
\code{getMaxIndexOfCols} returns the index of the maximal
element of each col. \code{getMinIndexOfCols} returns the
index of the minimal element of each col. If a
corresponding vector (row or col) is empty, possibly after
NA removal, -1 is returned as index.
}
\examples{
x = matrix(runif(5 * 3), ncol=3)
print(x)
print(getMaxIndexOfRows(x))
print(getMinIndexOfRows(x))
}

