#' Check subset relation on two vectors.
#'
#' @param x [\code{vector}]\cr
#'   Source vector.
#' @param y [\code{vector}]\cr
#'   Vector of the same mode as \code{x}.
#' @param strict [\code{logical(1)}]\cr
#'   Checks for strict/proper subset relation.
#' @return [\code{logical(1)}]
#'   \code{TRUE} if each element of \code{x} is also contained in \code{y}, i. e.,
#'   if \code{x} is a subset of \code{y} and \code{FALSE} otherwise.
#' @export
is.subset = function(x, y, strict = FALSE) {
  if (length(x) == 0)
    return(TRUE)
  res = all(x %in% y)
  if (strict)
    res = res & !is.subset(y, x)
  return(res)
}
