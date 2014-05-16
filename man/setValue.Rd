\name{setValue}
\alias{setValue}
\title{Set a list element to a new Value. This wrapper supports setting elements to \code{NULL}.}
\usage{
setValue(obj, index, newVal)
}
\arguments{
  \item{obj}{[\code{list}]\cr}

  \item{index}{[\code{character} | \code{integer}]\cr Index
  where to put the new Value(s).}

  \item{newVal}{[mixed] Will be wrapped in a new list
  element. Has to be a list if \code{index} is a vector.}
}
\value{
[\code{list}]
}
\description{
Set a list element to a new Value. This wrapper supports
setting elements to \code{NULL}.
}

