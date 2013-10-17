\name{namedList}
\alias{namedList}
\title{Create named list, possibly initialized with a certain element.}
\usage{
  namedList(names, init)
}
\arguments{
  \item{names}{[\code{character}]\cr Names of elements.}

  \item{init}{[valid R expression]\cr If given all list
  elements are initialized to this, otherwise \code{NULL}
  is used.}
}
\value{
  [\code{list}].
}
\description{
  Create named list, possibly initialized with a certain
  element.
}
\examples{
namedList(c("a", "b"))
namedList(c("a", "b"), init=1)
}

