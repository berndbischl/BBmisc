\name{printStrToChar}
\alias{printStrToChar}
\title{Print \code{str(x)} of an object to a string / character vector.}
\usage{
printStrToChar(x, collapse = "\\n")
}
\arguments{
  \item{x}{[any]\cr Object to print}

  \item{collapse}{[\code{character(1)}]\cr Used to collapse
  multiple lines.  \code{NULL} means no collapsing, vector
  is returned.  Default is \dQuote{\\n}.}
}
\value{
[\code{character}].
}
\description{
Print \code{str(x)} of an object to a string / character
vector.
}
\examples{
printStrToChar(iris)
}

