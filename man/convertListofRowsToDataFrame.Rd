\name{convertListofRowsToDataFrame}
\alias{convertListofRowsToDataFrame}
\title{Convert a list of row-lists of equal structure to a data.frame.}
\usage{
convertListofRowsToDataFrame(rows, force.names = FALSE,
  strings.as.factors = default.stringsAsFactors(), row.names = NULL,
  col.names)
}
\arguments{
  \item{rows}{[\code{list}]\cr List of list of rows, where
  each inner list is of the same structure.}

  \item{force.names}{[\code{logical(1)}]\cr Convert
  character columns to factors?  Default is
  \code{default.stringsAsFactors()}.}

  \item{strings.as.factors}{[\code{logical(1)}]\cr Convert
  character columns to factors?  Default is
  \code{default.stringsAsFactors()}.}

  \item{row.names}{[\code{character(nrow)} |
  \code{NULL}]\cr Row names.  Default is \code{NULL}.}

  \item{col.names}{[\code{character(ncol)}]\cr Column
  names.  Default is \dQuote{V1}, \dQuote{V2}, and so on.}
}
\value{
[\code{data.frame}].
}
\description{
Convert a list of row-lists of equal structure to a
data.frame.
}
\examples{
convertListofRowsToDataFrame(list(list(x=1, y="a"), list(x=2, y="b")))
}

