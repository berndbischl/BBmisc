\name{coalesce}
\alias{coalesce}
\title{Returns first non-missing, non-null argument.}
\usage{
coalesce(...)
}
\arguments{
  \item{...}{[any]\cr Arguments.}
}
\value{
[any].
}
\description{
Returns first non-missing, non-null argument, otherwise
\code{NULL}.
}
\examples{
f = function(x,y) {
  print(coalesce(NULL, x, y))
}
f(y=3)
}

