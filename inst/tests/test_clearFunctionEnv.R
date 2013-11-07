#' Clear the environment of a function
#'
#' @param f [\code{function}]\cr
#'   Function whose environment to clear.
#' @return Function with empty environment, parent is \code{.GlobalEnv}.
#' @export
#' @examples
#' get_f = function() { x = 1; function() x }
#' f = get_f()
#' print(ls(environment(f)))
#' f = clearFunctionEnv(f)
#' print(ls(environment(f)))
clearFunctionEnv = function(f) {
  f = match.fun(f)
  environment(f) = new.env(parent=.GlobalEnv)
  f
}
