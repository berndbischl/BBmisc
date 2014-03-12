\name{asMatrixCols}
\alias{asMatrixCols}
\alias{asMatrixRows}
\title{Extracts a named element from a list of lists.}
\usage{
asMatrixCols(xs, row.names, col.names)

asMatrixRows(xs, row.names, col.names)
}
\arguments{
  \item{xs}{[\code{list}]\cr A list of vectors of the same
  length.}

  \item{row.names}{[\code{character(1)}]\cr Row names of
  result.  Default is to take the names of the elements of
  \code{xs}.}

  \item{col.names}{[\code{character(1)}]\cr Column names of
  result.  Default is to take the names of the elements of
  \code{xs}.}
}
\value{
[\code{matrix}].
}
\description{
Extracts a named element from a list of lists.
}

