\name{computeMode}
\alias{computeMode}
\title{Compute statistical mode of a vector (value that occurs most frequently).}
\usage{
computeMode(x, ties.method = "random", na.rm = TRUE)
}
\arguments{
  \item{x}{[\code{vector}]\cr Factor, character, integer,
  numeric or logical vector.}

  \item{na.rm}{[\code{logical(1)}]\cr If \code{TRUE},
  missing values in the data removed.  if \code{FALSE},
  they are used as a separate level and this level could
  therefore be returned as the most frequent one.  Default
  is \code{TRUE}.}

  \item{ties.method}{[\code{character(1)}]\cr
  \dQuote{first}, \dQuote{random}, \dQuote{last}: Decide
  which value to take in case of ties.  Default is
  \dQuote{random}.}
}
\value{
Modal value of length 1, data type depends on data type of
x.
}
\description{
Works for integer, numeric, factor and character vectors.
The implementation is currently not extremely efficient.
}
\examples{
computeMode(c(1,2,3,3))
}

