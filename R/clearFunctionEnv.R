#' Clear the environment of a function
#'
#' @param f [\code{function}]\cr
#'   Function whose environment to clear.
#' @param inplace [\code{logical(1)}]\cr
#'   If \code{TRUE}, remove all object from the function's environment.
#'   Assign a new one and let the garbage collector take care otherwise.
#'   If this is environment is the global environment, a new one will be assigned
#'   either way.
#'   Default is \code{FALSE}.
#' @return Function with empty environment, parent is \code{.GlobalEnv}.
#' @export
#' @examples
#' get_f = function() { x = 1; function() x }
#' f = get_f()
#' print(ls(environment(f)))
#' f = clearFunctionEnv(f)
#' print(ls(environment(f)))
clearFunctionEnv = function(f, inplace=FALSE) {
  f = match.fun(f)
  if (inplace) {
    ee = environment(f)
    if (!environmentName(ee) == "R_GlobalEnv") {
      rm(list=ls(ee, all.names=TRUE), envir=ee)
      return(f)
    }
  }

  environment(f) = new.env(parent=.GlobalEnv)
  f
}
