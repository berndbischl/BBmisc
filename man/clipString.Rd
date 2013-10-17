\name{clipString}
\alias{clipString}
\title{Shortens a string to a given length.}
\usage{
  clipString(x, len, tail = "...")
}
\arguments{
  \item{x}{[\code{character(1)}]\cr String.}

  \item{len}{[\code{integer(1)}]\cr Absolute length the
  string should be clipped to, including \code{tail}.  Note
  that you cannot clip to a shorter length than
  \code{tail}.}

  \item{tail}{[\code{character(1)}]\cr If the string has to
  be shortened at least 1 character, the final characters
  will be \code{tail}.  Default is \dQuote{...}.}
}
\value{
  [\code{character(1)}].
}
\description{
  Shortens a string to a given length.
}
\examples{
print(clipString("abcdef", 10))
print(clipString("abcdef", 5))
}

