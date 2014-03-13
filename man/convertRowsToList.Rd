\name{convertRowsToList}
\alias{convertColsToList}
\alias{convertRowsToList}
\title{Convert rows of data.frame or matrix to lists.}
\usage{
convertRowsToList(x, use.names = FALSE, factors.as.char = TRUE,
  as.vector = TRUE)

convertColsToList(x, use.names = FALSE, factors.as.char = TRUE,
  as.vector = TRUE)
}
\arguments{
  \item{x}{[\code{matrix} | \code{data.frame}]\cr Object to
  convert.}

  \item{use.names}{[\code{logical(1)}]\cr Name elements of
  lists with column names of \code{x}.  Default is
  \code{FALSE}.}

  \item{factors.as.char}{[\code{logical(1)}]\cr If \code{x}
  is a data.frame, convert factor columns to string
  elements in the resulting lists?  Default is
  \code{TRUE}.}

  \item{as.vector}{[\code{logical(1)}]\cr If \code{x} is a
  matrix, store rows as vectors in the resulting list - or
  otherwise as lists?  Default is \code{TRUE}.}
}
\value{
[\code{list} of lists or vectors].
}
\description{
For each row, one list/vector is constructed, each entry of
the row becomes a list/vector element.
}

