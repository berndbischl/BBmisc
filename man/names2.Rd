\name{names2}
\alias{names2}
\title{Replacement for names which always returns a vector.}
\usage{
names2(x, missing.val = NA_character_)
}
\arguments{
  \item{x}{[\code{ANY}]\cr Object, probably named.}

  \item{missing.val}{[\code{ANY}]\cr Value to set for
  missing names. Default is \code{NA_character_}.}
}
\value{
[\code{character}]: vector of the same length as \code{x}.
}
\description{
A simple wrapper for \code{\link[base]{names}}. Returns a
vector even if no names attribute is set. Values \code{NA}
and \code{""} are treated as missing and replaced with the
value provided in \code{missing.val}.
}
\examples{
x = 1:3
names(x)
names2(x)
names(x[1:2]) = letters[1:2]
names(x)
names2(x)
}

