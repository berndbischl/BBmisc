\name{clearFunctionEnv}
\alias{clearFunctionEnv}
\title{Clear the environment of a function}
\usage{
  clearFunctionEnv(f, inplace = FALSE)
}
\arguments{
  \item{f}{[\code{function}]\cr Function whose environment
  to clear.}

  \item{inplace}{[\code{logical(1)}]\cr If \code{TRUE},
  remove all object from the function's environment.
  Assign a new one and let the garbage collector take care
  otherwise.  If this is environment is the global
  environment, a new one will be assigned either way.
  Default is \code{FALSE}.}
}
\value{
  Function with empty environment, parent is
  \code{.GlobalEnv}.
}
\description{
  Clear the environment of a function
}
\examples{
get_f = function() { x = 1; function() x }
f = get_f()
print(ls(environment(f)))
f = clearFunctionEnv(f)
print(ls(environment(f)))
}

