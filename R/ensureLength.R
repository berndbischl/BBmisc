#' Ensure that an input is a vector / list of a certain length.
#'
#' Useful for standard argument conversion where a user can input a single
#' element, but this has to be replicated now n times for a resulting vector or list.
#'
#' @param x [any]\cr
#'   Input element.
#' @param n [\code{integer}]\cr
#'   Desired length.
#' @param names [\code{character}]\cr
#'   Names for result.
#'   Default is NULL, which means no names.
#' @return [\code{vector} | \code{list}].
#' @export
ensureLength = function(x, n, names = NULL) {
  n = convertInteger(n)
  checkArg(n, "integer", len = 1L, na.ok = FALSE)
  if (isScalarValue(x))
    xs = rep(x, n)
  else
    xs = replicate(n, x, simplify = FALSE)
  if (!is.null(names)) {
    checkArg(names, "character", len = n, na.ok = FALSE)
    names(xs) = names
  }
  return(xs)
}

