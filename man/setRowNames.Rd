\name{setRowNames}
\alias{setColNames}
\alias{setRowNames}
\title{Wrapper for \code{rownames(x) = y}, \code{colnames(x) = y}.}
\usage{
setRowNames(x, names)

setColNames(x, names)
}
\arguments{
  \item{x}{[\code{matrix} | \code{data.frame}]\cr Matrix or
  data.frame.}

  \item{names}{[\code{character}]\cr New names for rows /
  columns.}
}
\value{
Changed object \code{x}.
}
\description{
Wrapper for \code{rownames(x) = y}, \code{colnames(x) = y}.
}
\examples{
setColNames(matrix(1:4, 2, 2), c("a", "b"))
}

