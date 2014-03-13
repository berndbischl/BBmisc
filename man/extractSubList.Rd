\name{extractSubList}
\alias{extractSubList}
\title{Extracts a named element from a list of lists.}
\usage{
extractSubList(xs, element, element.value, simplify = TRUE,
  use.names = TRUE)
}
\arguments{
  \item{xs}{[\code{list}]\cr A list of named lists.}

  \item{element}{[\code{character(1)}]\cr Name of element
  to extract from the list elements of \code{xs}.}

  \item{element.value}{[any]\cr If given,
  \code{\link{vapply}} is used and this argument is passed
  to \code{FUN.VALUE}.}

  \item{simplify}{[\code{logical(1)} | character(1)]\cr If
  \code{FALSE} \code{\link{lapply}} is used, otherwise
  \code{\link{sapply}}.  If \dQuote{cols}, we expect the
  elements to be vectors of the same length and they are
  arranged as the columns of the resulting matrix.  If
  \dQuote{rows}, likewise, but rows of the resulting
  matrix.  Default is \code{TRUE}.}

  \item{use.names}{[\code{logical(1)}]\cr If \code{TRUE}
  and \code{xs} is named, the result is named as \code{xs},
  otherwise the result is unnamed.  Default is
  \code{TRUE}.}
}
\value{
[\code{list} | simplified \code{vector} | \code{matrix}].
See above.
}
\description{
Extracts a named element from a list of lists.
}
\examples{
xs = list(list(a=1, b=2), list(a=5, b=7))
extractSubList(xs, "a")
extractSubList(xs, "a", simplify=FALSE)
}

