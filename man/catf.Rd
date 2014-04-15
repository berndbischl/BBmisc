\name{catf}
\alias{catf}
\title{Wrapper for cat and sprintf.}
\usage{
catf(..., file = "", append = FALSE, newline = TRUE)
}
\arguments{
  \item{...}{[any]\cr See \code{\link{sprintf}}.}

  \item{file}{[\code{character(1)}]\cr See
  \code{\link{cat}}.  Default is \dQuote{}.}

  \item{append}{[\code{logical(1)}]\cr See
  \code{\link{cat}}.  Default is \code{FALSE}.}

  \item{newline}{[\code{logical(1)}]\cr Append newline at
  the end?  Default is \code{TRUE}.}
}
\value{
Nothing.
}
\description{
A simple wrapper for \code{cat(sprintf(...))}.
}
\examples{
msg = "a message."
catf("This is \%s", msg)
}

