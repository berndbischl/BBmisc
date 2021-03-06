% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rangeVal.R
\name{rangeVal}
\alias{rangeVal}
\title{Calculate range statistic.}
\usage{
rangeVal(x, na.rm = FALSE)
}
\arguments{
\item{x}{[\code{numeric}]\cr
The vector.}

\item{na.rm}{[\code{logical(1)}]\cr
If \code{FALSE}, NA is returned if an NA is encountered in \code{x}.
If \code{TRUE}, NAs are disregarded.
Default is \code{FALSE}}
}
\value{
[\code{numeric(1)}].
}
\description{
A simple wrapper for \code{diff(range(x))}, so \code{max(x) - min(x)}.
}
