\name{dropColumns}
\alias{dropColumns}
\title{Drop columns of data frames or matricies}
\usage{
  dropColumns(x, cols)
}
\arguments{
  \item{x}{[\code{matrix} or \code{data.frame}]\cr Matrix
  or data frame for which columns should be removed.}

  \item{cols}{[\code{character}, \code{numeric} or
  \code{logical}]\cr Index specifing the columns to remove.
  The negation will be passed together with
  \code{drop=FALSE} to the index operator.}
}
\value{
  [\code{matrix} or \code{data.frame}]: Matrix or data
  frame with dropped columns.
}
\description{
  Drop columns of data frames or matricies
}
\examples{
dropColumns(iris, "Species")
dropColumns(iris, 4)
dropColumns(iris, -(1:3))
}

