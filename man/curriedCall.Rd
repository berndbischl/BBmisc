\name{curriedCall}
\alias{curriedCall}
\title{Call a function using lazy evaluated args}
\usage{
  curriedCall(fun, args = list(), ...)
}
\arguments{
  \item{fun}{[\code{function}]\cr Function to call.}

  \item{args}{[\code{list}]\cr List of function arguments,
  default is the empty list \code{list()}.}

  \item{...}{[any]\cr Additional arguments passed to
  \code{fun}, but without the need to put them in a list
  first (like \code{do.call} requires it) which always
  inflicts a copy.}
}
\description{
  Call a function using lazy evaluated args
}

