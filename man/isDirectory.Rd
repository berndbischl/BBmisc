\name{isDirectory}
\alias{isDirectory}
\title{Is one / are several files a directory?}
\usage{
isDirectory(...)
}
\arguments{
  \item{...}{[\code{character(1)}]\cr File names, all
  strings.}
}
\value{
[\code{logical}].
}
\description{
If a file does not exist, \code{FALSE} is returned.
}
\examples{
print(isDirectory(tempdir()))
print(isDirectory(tempfile()))
}

