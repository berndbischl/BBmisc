\name{convertRowsToList}
\alias{convertRowsToList}
\title{Convert rows of data.frame or matrix to lists.}
\usage{
  convertRowsToList(x, use.names = FALSE,
    factors.as.char = TRUE)
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
}
\value{
  [\code{list} of lists].
}
\description{
  For each row, one list is constructed, each entry of the
  row becomes a separate list element.
}

