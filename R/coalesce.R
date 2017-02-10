#' @title Returns first non-missing, non-null argument.
#'
#' @description
#' Returns first non-missing, non-null argument, otherwise \code{NULL}.
#'
#' We have to perform some pretty weird \code{\link{tryCatch}} stuff internally,
#' so you should better not pass complex function calls into the arguments that can throw exceptions,
#' as these will be completely muffled, and return \code{NULL} in the end.
#'
#' @param ... [any]\cr
#'   Arguments.
#' @return [any].
#' @export
#' @examples
#' f = function(x,y) {
#'   print(coalesce(NULL, x, y))
#' }
#' f(y = 3)
coalesce = function(...) {
  dots = match.call(expand.dots = FALSE)$...
  for (arg in dots) {
    ismissing = if (is.symbol(arg)) {
      eval(substitute(missing(symbol), list(symbol = arg)), envir = parent.frame())
    } else {
      FALSE
    }
    if (!ismissing) {
      value = tryCatch(eval(arg, envir = parent.frame()), error = function(...) NULL)
      if (!is.null(value)) {
        return(value)
      }
    }
  }
  NULL
}
