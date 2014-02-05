\name{convertMatrixType}
\alias{convertMatrixType}
\title{Converts storage type of a matrix.}
\usage{
convertMatrixType(x, type)
}
\arguments{
  \item{x}{[\code{matrix}]\cr.  Matrix to convert.}

  \item{type}{[\code{character(1)}]\cr New storage type.}
}
\value{
[\code{matrix}].
}
\description{
Works by setting \code{\link{mode}}.
}
\note{
\code{as.mytype} drops dimension when used on a matrix.
}

