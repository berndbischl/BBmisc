\name{\%is\%}
\alias{\%is\%}
\title{Binary operator which acts as a wrapper for \code{\link[base]{inherits}}.}
\usage{
object \%is\% class2
}
\arguments{
  \item{object}{[\code{any}]\cr A R object.}

  \item{class2}{[\code{character(1)}]\cr A character vector
  naming the class.}
}
\value{
[\code{logical(1)}] Returns \code{TRUE} if object is of the
corresponding class and \code{FALSE} otherwise.
}
\description{
Binary operator which acts as a wrapper for
\code{\link[base]{inherits}}.
}

