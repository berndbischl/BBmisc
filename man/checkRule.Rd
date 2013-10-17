\name{checkRule}
\alias{assertRule}
\alias{checkRule}
\title{Checks argument against a rule.}
\usage{
  checkRule(x, rule)

  assertRule(x, rule)
}
\arguments{
  \item{x}{[ANY]\cr Object the check.}

  \item{rule}{[\code{character(1)}]\cr Rule. See details.}
}
\value{
  [logical(1)] \code{TRUE} if \code{x} complies to
  \code{rule}, \code{FALSE} (or an error) otherwise.
}
\description{
  The functions check an object \code{x} against a
  specified rule which allows to express frequent argument
  checks by just some letters letters. If the object
  complies to the specified rule, \code{TRUE} is returned
  by both functions. Otherwise \dQuote{checkRule} returns
  \code{FALSE} and \dQuote{assertRule} throws an
  informative errror message. The rule to check \code{x}
  against must be given in the simple format
  \dQuote{[type][length]} explained in the details.
}
\details{
  Recognized type characters: \tabular{rl}{ \code{[bB]}
  \tab Checks for type bool / logical. Uppercase character
  enforces a checks for no NAs.\cr \code{[iI]} \tab Checks
  for type integer. Uppercase character enforces a checks
  for no NAs.\cr \code{[rR]} \tab Checks for type real /
  double. Uppercase character enforces a checks for no NAs
  or NaNs.\cr \code{[cC]} \tab Checks for type complex.
  Uppercase character enforces a checks for no NAs or
  NaNs.\cr \code{[nN]} \tab Checks for type numeric
  (integer or double). Uppercase character enforces a
  checks for no NAs or NaNs.\cr \code{[sS]} \tab Checks for
  type string (character). Uppercase character enforces a
  checks for no NAs.\cr \code{[aA]} \tab Checks for type
  atomic. Uppercase character enforces a checks for no NAs
  or NaNs.\cr \code{[lL]} \tab Checks for type list.
  Uppercase character enforces a checks for no missing list
  elements (\code{NULL}).\cr \code{[mM]} \tab Checks for
  type matrix Uppercase character enforces a checks for no
  NAs or NaNs.\cr \code{[f]} \tab Checks for type
  function.\cr \code{[e]} \tab Checks for type
  environment.\cr \code{[d]} \tab Checks for type
  data.frame.\cr \code{[0]} \tab Checks for \code{NULL}.\cr
  \code{[*]} \tab Any type. }

  The length rule can be omitted (resulting in no length
  check) or can be provied by \tabular{rl}{ \code{[*]} \tab
  Any length.\cr \code{[?]} \tab Length of zero or one.\cr
  \code{[+]} \tab Length of at least one.\cr \code{[0-9]+}
  \tab Exact length given as integer. }
}
\note{
  The functions are inspired by the blog post of Bogumił
  Kamiński:
  \url{http://rsnippets.blogspot.de/2013/06/testing-function-agruments-in-gnu-r.html}.
  The functionallity is slightly modified and expanded to
  check more types. The implementation is written in
  \code{C} to minimize caused overhead.
}
\examples{
# simple flag? (scalar logical, not NA)
checkRule(TRUE, "B1")

# non-empty numeric with no NAs?
xs = rnorm(5)
checkRule(xs, "N+")
xs[3] = NA
checkRule(xs, "N+")
}

