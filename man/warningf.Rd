\name{warningf}
\alias{warningf}
\title{Wrapper for warning and sprintf.}
\usage{
warningf(..., immediate = TRUE, warning.length = 8170L)
}
\arguments{
  \item{...}{[any]\cr See \code{\link{sprintf}}.}

  \item{immediate}{[\code{logical(1)}]\cr See
  \code{\link{warning}}.  Default is \code{TRUE}.}

  \item{warning.length}{[\code{integer(1)}]\cr Number of
  chars after which the warning message gets truncated, see
  ?options.  Default is 8170.}
}
\value{
Nothing.
}
\description{
A wrapper for \code{\link{warning}} with
\code{\link{sprintf}} applied to the arguments.
}
\examples{
msg <- "a warning"
warningf("this is \%s", msg)
}

