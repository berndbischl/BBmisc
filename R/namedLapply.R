#' @title Like \code{lapply} but also passes the name to the function.
#'
#' @param x [\code{list}]\cr
#'   Named list.
#' @param fun [\code{function}]\cr
#'   The function to apply. Second argument will be the name of the element.
#' @param ... [any]\cr
#'   Further arguments passed down to \code{fun}.
#' @export
#' @return [\code{list}].
namedLapply = function(x, fun, ...) {
  assertList(x)
  assertFunction(fun)
  x.names = names(x)
  x = lapply(seq_along(x), function(i) fun(x[[i]], x.names[i], ...))
  setNames(x, x.names)
}