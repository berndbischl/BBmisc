\name{dropNamed}
\alias{dropNamed}
\title{Drop named elements of an object}
\usage{
  dropNamed(x, drop)
}
\arguments{
  \item{x\cr}{Object to drop named elements from.  For
  matrix and data frames this function delegates to drop
  using the second dimension (columns).  The normal index
  operator \dQuote{[} is applied otherwise otherwise.}

  \item{drop}{[\code{character}]\cr Names to drop.}
}
\value{
  Subset of object of same type as \code{x}.
}
\description{
  Drop named elements of an object
}

