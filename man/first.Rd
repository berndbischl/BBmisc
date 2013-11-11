\name{first}
\alias{first}
\alias{last}
\title{Find the index of first/last \code{TRUE} value in a logical vector.}
\usage{
  first(x, na.omit = TRUE)

  last(x, na.omit = TRUE)
}
\arguments{
  \item{x}{[\code{logical}]\cr Logical vector.}
}
\value{
  Returns a scalar integer of the index of the first/last
  \code{TRUE} value in \code{x} or an empty integer vector
  if none is found.
}
\description{
  Find the index of first/last \code{TRUE} value in a
  logical vector.
}

