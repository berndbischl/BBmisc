\name{stopf}
\alias{stopf}
\title{Wrapper for stop and sprintf.}
\usage{
stopf(..., warning.length = 8170L)
}
\arguments{
  \item{...}{[any]\cr See \code{\link{sprintf}}.}

  \item{warning.length}{[\code{integer(1)}]\cr Number of
  chars after which the error message gets truncated, see
  ?options.  Default is 8170.}
}
\value{
Nothing.
}
\description{
A wrapper for \code{\link{stop}} with \code{\link{sprintf}}
applied to the arguments. Notable difference is that error
messages are not truncated to 1000 characters by default.
}
\examples{
err = "an error."
try(stopf("This is \%s", err))
}

