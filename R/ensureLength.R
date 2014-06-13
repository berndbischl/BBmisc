#' Ensure that an input is a vector / list of a certain length.
#'
#' Useful for standard argument conversion where a user can input a single 
#' element, but this has to be replicated now n times.
#'
#' @param x [any]\cr
#'   Input element.
#' @param n [\code{integer}]\cr
#'   Desired length.
#' @return [\code{vector} | \code{list}].
#' @export
ensureLength = function(...) {
  if (isScalarValue(x))
    rep(x, )
  args = lapply(list(...), as.factor)
  newlevels = sort(unique(unlist(lapply(args, levels))))
  ans = unlist(lapply(args, function(x) {
        m = match(levels(x), newlevels)
        m[as.integer(x)]
      }))
  levels(ans) = newlevels
  setClasses(ans, "factor")
}

