\name{getOperatingSystem}
\alias{getOperatingSystem}
\alias{isUnix}
\alias{isWindows}
\title{Functions to determine the operating system.}
\usage{
  getOperatingSystem()

  isWindows()

  isUnix()
}
\value{
  See above.
}
\description{
  \itemize{ \item{getOperatingSystem}{Simple wrapper for
  \code{.Platform$OS.type}, returns \code{character(1)}.}
  \item{isUnix}{Predicate for OS string, returns
  \code{logical(1)}. Currently this would include Unix,
  Linux and Mac flavours.} \item{isWindows}{Predicate for
  OS string, returns \code{logical(1)}.} }
}

