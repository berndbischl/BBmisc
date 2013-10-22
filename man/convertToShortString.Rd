\name{convertToShortString}
\alias{convertToShortString}
\title{Converts an arbitrary R object to a short, descriptive string so it can be used in messages.}
\usage{
  convertToShortString(x, num.format = "\%.4g")
}
\arguments{
  \item{x}{[any]\cr The object.}

  \item{num.format}{[\code{character(1)}]\cr Used to format
  numerical scalars via \code{\link{sprintf}}.  Default is
  \dQuote{\%.4g}.}
}
\value{
  [\code{character(1)}].
}
\description{
  Atomics: If of length 0 or 1, they are basically printed
  as they are. Numerics are formated with
  \code{num.format}. If of length greater than 1, their
  \code{str} is printed in a way so it does not become
  excessively long.
}
\details{
  All others: Currently, only their class is simply printed
  like \dQuote{<data.frame>}.

  Lists: The mechanism above is applied (non-recursively)
  to their elements. The result looks like this:
  \dQuote{a=1, <unamed>=2, b=<data.frame>, c=<list>}.
}
\examples{
listToShortString(list(a=1, b=NULL, "foo", c=1:10))
}

