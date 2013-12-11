\name{do.call2}
\alias{do.call2}
\title{Execute a function call}
\usage{
do.call2(fun, ..., .args = list())
}
\arguments{
  \item{fun}{[\code{function}]\cr Function to call.}

  \item{...}{[any]\cr Arguments to \code{fun}. Best
  practice is to specify them in a \code{key=value}
  syntax.}

  \item{.args}{[\code{list}]\cr Arguments to \code{fun} as
  a (named) list. Will be passed after arguments in
  \code{...}.  Default is \code{list()}.}
}
\description{
This function is supposed to be a replacement for
\code{\link[base]{do.call}} in situations where you need to
pass big R objects which can simply be passed via
\code{...} without inflicting a copy.
}

