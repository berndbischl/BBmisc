\name{listToShortString}
\alias{listToShortString}
\title{Converts list to short string describing contents.}
\usage{
  listToShortString(xs, num.format = "\%.4g")
}
\arguments{
  \item{xs}{[\code{list}]\cr The list.}

  \item{num.format}{[\code{list}]\cr Used to format
  numerical scalars via \code{\link{sprintf}}.  Default is
  \dQuote{\%.4g}.}
}
\value{
  [\code{character(1)}].
}
\description{
  Looks like this \dQuote{a=1,<unamed>=2,b=<data.frame>}.
  Vectors are displayed up to a certain length.
}
\examples{
listToShortString(list(a=1, b=NULL, "foo", c=1:10))
}

