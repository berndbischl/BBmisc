\name{setAttribute}
\alias{setAttribute}
\title{A wrapper for \code{attr(x, which) = y}.}
\usage{
setAttribute(x, which, value)
}
\arguments{
  \item{x}{[any]\cr Your object.}

  \item{which}{[\code{character(1)}]\cr Name of the
  attribute to set}

  \item{value}{[\code{ANY}]\cr Value for the attribute.}
}
\value{
Changed object \code{x}.
}
\description{
A wrapper for \code{attr(x, which) = y}.
}
\examples{
setAttribute(list(), "foo", 1)
}

