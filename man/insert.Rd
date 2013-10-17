\name{insert}
\alias{insert}
\title{Insert elements from one list into another list.}
\usage{
  insert(xs1, xs2, elements)
}
\arguments{
  \item{xs1}{[\code{list}]\cr First list.}

  \item{xs2}{[\code{list}]\cr Second list. Must be fully
  and uniquely named.}

  \item{elements}{[\code{character}]\cr Elements from
  \code{xs2} to insert into \code{xs1}.  Default is all.}
}
\value{
  [\code{list}]. \code{x1} with replaced elements from
  \code{x2}.
}
\description{
  Inserts elements from list \code{x2} into list \code{x1}
  by name, overwriting elements of equal names.
}
\examples{
xs1 <- list(a=1, b=2)
xs2 <- list(b=1, c=4)
insert(xs1, xs2)
insert(xs1, xs2, elements="c")
}

