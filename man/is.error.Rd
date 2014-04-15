\name{is.error}
\alias{is.error}
\title{Is return value of try an exception?}
\usage{
is.error(x)
}
\arguments{
  \item{x}{[any]\cr Any object, usually the return value of
  \code{\link[base]{try}}, \code{\link[base]{tryCatch}}, or
  a function which may return a
  \code{\link[base]{simpleError}}.}
}
\value{
[\code{logical(1)}].
}
\description{
Checks if an object is of class \dQuote{try-error} or
\dQuote{error}.
}
\examples{
x = try(stop("foo"))
print(is.error(x))
x = 1
print(is.error(x))
}

