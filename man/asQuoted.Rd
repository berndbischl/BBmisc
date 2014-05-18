\name{asQuoted}
\alias{asQuoted}
\title{Converts a string into a quoted expression.}
\usage{
asQuoted(s, env = parent.frame())
}
\arguments{
  \item{s}{[\code{character(1)}]\cr Expression as string.}

  \item{env}{[\code{numeric(1)}]\cr Environment for
  expression.  Default is \code{parent.frame()}}
}
\value{
Quoted expression.
}
\description{
Works the same as if you would have entered the expression and called
\code{\link{quote}} on it.
}
\examples{
asQuoted("x == 3")
}

