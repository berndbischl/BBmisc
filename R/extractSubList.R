#' Extracts a named element from a list of lists.
#'
#' @param xs [\code{list}]\cr
#'   A list of named lists.
#' @param element [\code{character(1)}]\cr
#'   Name of element to extract from the list elements of \code{xs}.
#' @param element.value [any]\cr
#'   If given, \code{\link{vapply}} is used and this argument is passed to \code{FUN.VALUE}.
#' @param simplify [\code{logical(1)} | character(1)]\cr
#'   If \code{FALSE} \code{\link{lapply}} is used, otherwise \code{\link{sapply}}.
#'   If \dQuote{cols}, we expect the elements to be vectors of the same length and they are
#'   arranged as the columns of the resulting matrix.
#'   If \dQuote{rows}, likewise, but rows of the resulting matrix.
#'   Default is \code{TRUE}.
#' @param use.names [\code{logical(1)}]\cr
#'   If \code{TRUE} and \code{xs} is named, the result is named as \code{xs},
#'   otherwise the result is unnamed.
#'   Default is \code{TRUE}.
#' @return [\code{list} | simplified \code{vector} | \code{matrix}]. See above.
#' @export
#' @examples
#' xs = list(list(a=1, b=2), list(a=5, b=7))
#' extractSubList(xs, "a")
#' extractSubList(xs, "a", simplify=FALSE)
extractSubList = function(xs, element, element.value, simplify=TRUE, use.names=TRUE) {
  checkArg(xs, "list")
  checkArg(simplify, c("logical", "character"), len = 1L, na.ok = FALSE)
  if (is.character(simplify))
    checkArg(simplify, choices = c("cols", "rows"))
  else
    checkArg(simplify, "logical", len = 1L, na.ok = FALSE)
  checkArg(use.names, "logical", len = 1L, na.ok = FALSE)
  if (!missing(element.value)) {
    ys = vapply(xs, function(x) x[[element]], FUN.VALUE=element.value)
  } else if (isTRUE(simplify)) {
    ys = sapply(xs, function(x) x[[element]], USE.NAMES=use.names)
   } else {
    ys = lapply(xs, function(x) x[[element]])
    if (simplify == "rows")
      ys = asMatrixRows(ys)
    else if (simplify == "cols")
      ys = asMatrixCols(ys)
  }
  ns = names(xs)
  if (use.names && !is.null(ns))
    names(ys) = ns
  else
    names(ys) = NULL
  return(ys)
}
