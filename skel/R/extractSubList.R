#' Extracts a named element from a list of lists.
#' 
#' @param xs [\code{list}]\cr
#'   A list of named lists. 
#' @param element [\code{character(1)}]\cr
#'   Name of element to extract from the list elements of \code{xs}.
#' @param element.value [any]\cr
#'   If given, \code{\link{vapply}} is used and this argument is passed to \code{FUN.VALUE}.
#' @param simplify [\code{logical(1)}]\cr
#'   If \code{TRUE} \code{\link{sapply}} is used otherwise \code{\link{lapply}}.
#'   Default is \code{TRUE}.
#' @param use.names [\code{logical(1)}]\cr
#'   If \code{TRUE} and \code{xs} is named, the result is named as \code{xs}, 
#'   otherwise the result is unnamed.
#'   Default is \code{TRUE}.
#' @return [\code{list} or simplified vector].
#' @export
#' @examples
#' xs = list(list(a=1, b=2), list(a=5, b=7))
#' extractSubList(xs, "a")
#' extractSubList(xs, "a", simplify=FALSE)
extractSubList = function(xs, element, element.value, simplify=TRUE, use.names=TRUE) {
  if (!missing(element.value))  
    ys = vapply(xs, function(x) x[[element]], FUN.VALUE=element.value)
  else if (simplify)
    ys = sapply(xs, function(x) x[[element]], USE.NAMES=use.names)
  else 
    ys = lapply(xs, function(x) x[[element]])
  ns = names(xs)
  if (use.names && !is.null(ns))
    names(ys) = ns 
  else
    names(ys) = NULL
  return(ys)
}
