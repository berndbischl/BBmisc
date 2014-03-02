\name{dapply}
\alias{dapply}
\title{Call \code{lapply} on an object and return a data.frame.}
\usage{
dapply(x, fun, ..., col.names, quick = TRUE)
}
\arguments{
  \item{x}{[\code{data.frame}]\cr Data frame.}

  \item{fun}{[\code{function}]\cr The function to apply.}

  \item{...}{[any]\cr Further arguments passed down to
  \code{fun}.}

  \item{col.names}{[\code{character(1)}]\cr Column names
  for result.  Default are the names of \code{x}.}

  \item{quick}{[\code{logical(1)}]\cr ??????  Default is
  \code{TRUE}.}
}
\value{
[\code{data.frame}].
}
\description{
Call \code{lapply} on an object and return a data.frame.
}

