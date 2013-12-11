\name{messagef}
\alias{messagef}
\title{Wrapper for message and sprintf.}
\usage{
messagef(...)
}
\arguments{
  \item{...}{[any]\cr See \code{\link{sprintf}}.}
}
\value{
Nothing.
}
\description{
A simple wrapper for \code{message(sprintf(...))}.
}
\examples{
msg <- "a message"
warningf("this is \%s", msg)
}

