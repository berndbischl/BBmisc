#' Check if two values (or vecotrs of values) are equal.
#'
#' Whereas \code{NA == NA = TRUE} and \code{NA == 1 = FALSE}.
#'
#' @param x [\code{vector(n)}]\cr
#'   Value(s) that should be compared against \code{y}.
#' @param y [\code{vector(n)}]\cr
#'   Vector of the same length as \code{x}.
#' @return [\code{logical(n)}]. For each value in \code{x}: Is it the same as in \code{y}?
#' @export
#' @examples
#' x = c(1, 2, NA)
#' y = c(1L, 2L, NA)
#' isVectorEqual(x,y)
isVectorEqual = function(x, y) {
  assertVector(x)
  assertVector(y)
  if (length(x) != length(y)) {
    stopf("Vecors are not of the same length! x: %i, y: %i", length(x), length(y))
  }
  res = (x == y) | (is.na(x) & is.na(y))
  replace(res, is.na(res), FALSE)
}

