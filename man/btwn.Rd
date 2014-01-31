\name{\%btwn\%}
\alias{\%btwn\%}
\title{Check if some values are covered by the range of the values in a second vector.}
\usage{
x \%btwn\% y
}
\arguments{
  \item{x}{[\code{numeric(n)}]\cr Value(s) that should be
  within the range of \code{y}.}

  \item{y}{[\code{numeric}]\cr Numeric vector which defines
  the range.}
}
\value{
[\code{logical(n)}]. For each value in \code{x}: Is it in
the range of \code{y}?
}
\description{
Check if some values are covered by the range of the values
in a second vector.
}
\examples{
x = 3
y = c(-1,2,5)
x \%btwn\% y
}

