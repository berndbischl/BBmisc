#' Call a function using lazy evaluated args
#'
#' @param fun [\code{function}]\cr
#'   Function to call.
#' @param args [\code{list}]\cr
#'   List of function arguments, default is the empty list \code{list()}.
#' @param ... [any]\cr
#'   Additional arguments passed to \code{fun}, but without the need to
#'   put them in a list first (like \code{do.call} requires it) which
#'   always inflicts a copy.
#' @export
curriedCall = function(fun, args=list(), ...) {
  checkArg(fun, "function")
  checkArg(args, "list")
  do.call(CurryL(fun, ...), args)
}
