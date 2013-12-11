\name{setClasses}
\alias{setClasses}
\title{A wrapper for \code{class(x) = classes}.}
\usage{
setClasses(x, classes)
}
\arguments{
  \item{x}{[any]\cr Your object.}

  \item{classes}{[\code{character}]\cr New classes.}
}
\value{
Changed object \code{x}.
}
\description{
A wrapper for \code{class(x) = classes}.
}
\examples{
setClasses(list(), c("foo1", "foo2"))
}

