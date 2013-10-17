\name{any.missing}
\alias{any.missing}
\title{Check if an object contains missing values.}
\usage{
  any.missing(x, inf.as.missing = TRUE)
}
\arguments{
  \item{x}{[\code{ANY}]\cr Object to check.}

  \item{inf.as.missing}{[\code{logical(1)}]\cr Treat
  \code{Inf} and \code{-Inf} as missing? Default is
  code{TRUE}.}
}
\value{
  [\code{logical(1)}] Returns \code{TRUE} if at least one
  element of \code{x} was missing (see details).
}
\description{
  Supported are atomic types (see
  \code{\link[base]{is.atomic}}) and lists. Missingness is
  defined as values being \code{NA} or \code{NaN} for
  atomic types and \code{NULL} for lists.
}

