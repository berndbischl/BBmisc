\name{convertListOfRowsToDataFrame}
\alias{convertListOfRowsToDataFrame}
\title{Convert a list of row-vectors of equal structure to a data.frame.}
\usage{
convertListOfRowsToDataFrame(rows,
  strings.as.factors = default.stringsAsFactors(), row.names, col.names)
}
\arguments{
  \item{rows}{[\code{list}]\cr List of rows. Each row is a
  list or vector of the same structure.  That means all
  rows must have the same length and all corresponding
  elements must have the same class.}

  \item{strings.as.factors}{[\code{logical(1)}]\cr Convert
  character columns to factors?  Default is
  \code{default.stringsAsFactors()}.}

  \item{row.names}{[\code{character(nrow)} |
  \code{NULL}]\cr Row names for result.  \code{NULL}
  implies no row names are set.  By default the names of
  the list \code{rows} are taken.}

  \item{col.names}{[\code{character(ncol)}]\cr Column
  names.  Default the are the names of the elements of the
  first row, or, if this row has no element names,
  \dQuote{V1}, \dQuote{V2}, and so on, is used.}
}
\value{
[\code{data.frame}].
}
\description{
Convert a list of row-vectors of equal structure to a
data.frame.
}
\examples{
convertListOfRowsToDataFrame(list(list(x=1, y="a"), list(x=2, y="b")))
}

