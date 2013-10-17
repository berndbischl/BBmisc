\name{convertDataFrameCols}
\alias{convertDataFrameCols}
\alias{convertDfCols}
\title{Converts columns in a data frame to characters, factors or numerics.}
\usage{
  convertDataFrameCols(df, chars.as.factor = FALSE,
    factors.as.char = FALSE, ints.as.num = FALSE,
    logicals.as.factor = FALSE)

  convertDfCols(df, chars.as.factor = FALSE,
    factors.as.char = FALSE, ints.as.num = FALSE,
    logicals.as.factor = FALSE)
}
\arguments{
  \item{df}{[\code{data.frame}]\cr Data frame.}

  \item{chars.as.factor}{[\code{logical(1)}]\cr Should
  characters be converted to factors?  Default is
  \code{FALSE}.}

  \item{factors.as.char}{[\code{logical(1)}]\cr Should
  characters be converted to factors?  Default is
  \code{FALSE}.}

  \item{ints.as.num}{[\code{logical(1)}]\cr Should integers
  be converted to numerics?  Default is \code{FALSE}.}

  \item{logicals.as.factor}{[\code{logical(1)}]\cr Should
  logicals be converted to factors?  Default is
  \code{FALSE}.}
}
\value{
  [\code{data.frame}].
}
\description{
  \code{convertDfCols} is deprecated.
}

