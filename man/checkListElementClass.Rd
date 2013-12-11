\name{checkListElementClass}
\alias{checkListElementClass}
\title{Check that a list contains only elements of a required type.}
\usage{
checkListElementClass(xs, cl)
}
\arguments{
  \item{xs}{[\code{list}]\cr Argument.}

  \item{cl}{[\code{character(1)}]\cr Class that elements
  must have. Checked with \code{is}.}
}
\value{
Nothing.
}
\description{
Check that argument is a list and contains only elements of
a required type. Throws exception if check is not passed.
Note that argument is evaluated when checked.
}
\examples{
xs <- as.list(1:3)
checkListElementClass(xs, "numeric")
}

