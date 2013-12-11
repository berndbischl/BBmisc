\name{first}
\alias{first}
\alias{last}
\title{Find the index of first/last \code{TRUE} value in a logical vector.}
\usage{
first(x, na.omit = TRUE)

last(x, na.omit = TRUE)
}
\arguments{
  \item{x}{[\code{logical}]\cr Logical vector.}

  \item{na.omit}{[\code{logical(1)}]\cr Should NAs be
  omitted from \code{x}?.  Default is \code{TRUE}.}
}
\value{
Returns a scalar integer of the index of the first/last
\code{TRUE} value in \code{x} or an empty integer vector if
none is found.  If NAs are encountered before a \code{TRUE}
and not omitted, the result is \code{NA}.
}
\description{
Find the index of first/last \code{TRUE} value in a logical
vector.
}

