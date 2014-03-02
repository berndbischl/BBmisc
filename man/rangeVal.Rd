\name{rangeVal}
\alias{rangeVal}
\title{Calculate range statistic.}
\usage{
rangeVal(x)
}
\arguments{
  \item{x}{[\code{numeric}]\cr The vector.}
}
\value{
[\code{numeric(1)}].
}
\description{
A simple wrapper for \code{diff(range(x))}, so \code{max(x)
- min(x)}.
}

