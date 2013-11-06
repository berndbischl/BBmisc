\name{stringsAsFactors}
\alias{stringsAsFactors}
\title{Convert all character columns in data.frame to factors.}
\usage{
  stringsAsFactors(data, do = TRUE)
}
\arguments{
  \item{data}{[\code{data.frame}]\cr The data.frame.}

  \item{do}{[\code{logical(1)}]\cr Should the conversion be
  done?  Useful for omitting an if-statement in client
  code.  Default is \code{TRUE}}
}
\value{
  [\code{data.frame}].
}
\description{
  The function is DEPRECATED, use convertDataFrameCols!
}

