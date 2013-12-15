\name{isValidName}
\alias{isValidName}
\title{Can some strings be used for columns or list elements without problems?}
\usage{
isValidName(x, unique = TRUE)
}
\arguments{
  \item{x}{[\code{character}]\cr Character vector to
  check.}

  \item{unique}{[\code{logical(1)}]\cr Should the names be
  unique?  Default is \code{TRUE}.}
}
\value{
[\code{logical}]. One Boolean entry for each string in
\code{x}.  If the entries are not unique and this is
checked, the first duplicate will be \code{FALSE}.
}
\description{
Can some strings be used for columns or list elements
without problems?
}

