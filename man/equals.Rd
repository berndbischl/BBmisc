\name{\%equals\%}
\alias{\%equals\%}
\title{Check if two values (or vecotrs of values) are equal.}
\usage{
x \%equals\% y
}
\arguments{
  \item{x}{[\code{vector(n)}]\cr Value(s) that should be
  compared against \code{y}.}

  \item{y}{[\code{vector(n)}]\cr Vector of the same length
  as \code{x}.}
}
\value{
[\code{logical(n)}]. For each value in \code{x}: Is it the
same as in \code{y}?
}
\description{
Whereas \code{NA == NA = TRUE} and \code{NA == 1 = FALSE}.
}
\examples{
x = c(1, 2, NA)
y = c(1L, 2L, NA)
x \%equals\% y
}

