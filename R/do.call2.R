#FIXME:

# a) REALLY test this... 
# b) Proof that this increases performance with at least one example


#FIXME: bug with anonymous function?

#' Execute a function call
#'
#' This function is supposed to be a replacement for \code{\link[base]{do.call}} in situations
#' where you need to pass big R objects which can simply be passed via \code{...} without
#' inflicting a copy.
#'
#' @param fun [\code{function}]\cr
#'   Function to call.
#' @param ... [any]\cr
#'   Arguments to \code{fun}. Best practice is to specify them in a \code{key=value} syntax.
#' @param .args [\code{list}]\cr
#'   Arguments to \code{fun} as a (named) list. Will be passed after arguments in \code{...}.
#'   Default is \code{list()}.
#FIXME: return?
do.call2 = function(fun, ..., .args=list()) {
  checkArg(.args, "list")
  if (is.function(fun))
    fun = as.name(substitute(fun))
  else if (is.character(fun) && length(fun) == 1L && !is.na(fun))
    fun = as.name(fun)
  ddd = match.call(expand.dots=FALSE)$...
  expr = as.call(c(list(fun), ddd, .args))
  eval.parent(expr, n=1L)
}
