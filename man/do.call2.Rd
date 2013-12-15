\name{do.call2}
\alias{do.call2}
\title{Execute a function call}
\usage{
do.call2(fun, ..., .args = list())
}
\arguments{
  \item{fun}{[\code{character(1)}]\cr Name of the function
  to call.}

  \item{...}{[any]\cr Arguments to \code{fun}. Best
  practice is to specify them in a \code{key=value}
  syntax.}

  \item{.args}{[\code{list}]\cr Arguments to \code{fun} as
  a (named) list. Will be passed after arguments in
  \code{...}.  Default is \code{list()}.}
}
\value{
Return value of \code{fun}.
}
\description{
This function is supposed to be a replacement for
\code{\link[base]{do.call}} in situations where you need to
pass big R objects. Unlike \code{\link[base]{do.call}},
this function allows to pass objects via \code{...} to
avoid a copy.
}
\examples{
\dontrun {
  library(microbenchmark)
  x = 1:1e7
  microbenchmark(do.call(head, list(x, n=1)), do.call2("head", x, n=1))
}
}

