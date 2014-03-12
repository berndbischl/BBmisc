#' Extracts a named element from a list of lists.
#'
#' @param xs [\code{list}]\cr
#'   A list of named lists.
#' @param element [\code{character(1)}]\cr
#'   Name of element to extract from the list elements of \code{xs}.
#' @param element.value [any]\cr
#'   If given, \code{\link{vapply}} is used and this argument is passed to \code{FUN.VALUE}.
#' @param simplify [\code{logical(1)}]\cr
#'   If \code{TRUE} \code{\link{sapply}} is used, otherwise \code{\link{lapply}}.
#'   Default is \code{TRUE}.
#' @param use.names [\code{logical(1)}]\cr
#'   If \code{TRUE} and \code{xs} is named, the result is named as \code{xs},
#'   otherwise the result is unnamed.
#'   Default is \code{TRUE}.
#' @param matrix [\code{character(1)}]\cr
#'   If \dQuote{cols}, \code{\link{asMatrixCols}} is called on the result,
#'   if \dQuote{rows}, \code{\link{asMatrixRows}} is called on the result.
#'   \code{simplify = TRUE} is not used in this case.
#'   Default is not to do that.
#' @return [\code{list} | simplified \code{vector} | \code{matrix}]. See above.
#' @export
#' @examples
#' xs = list(list(a=1, b=2), list(a=5, b=7))
#' extractSubList(xs, "a")
#' extractSubList(xs, "a", simplify=FALSE)
extractSubList = function(xs, element, element.value, simplify=TRUE, use.names=TRUE, matrix) {
  checkArg(xs, "list")
  checkArg(simplify, "logical", len = 1L, na.ok = FALSE)
  checkArg(use.names, "logical", len = 1L, na.ok = FALSE)
  if (!missing(element.value))
    ys = vapply(xs, function(x) x[[element]], FUN.VALUE=element.value)
  else if (simplify && missing(matrix))
    ys = sapply(xs, function(x) x[[element]], USE.NAMES=use.names)
  else
    ys = lapply(xs, function(x) x[[element]])
  ns = names(xs)
  if (use.names && !is.null(ns))
    names(ys) = ns
  else
    names(ys) = NULL

  if (!missing(matrix)) {
    checkArg(matrix, choices = c("cols", "rows"))
    ys = if (matrix == "cols")
      asMatrixCols(ys)
    else
      asMatrixRows(ys)
  }
  return(ys)
}
