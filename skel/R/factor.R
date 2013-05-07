#' Combine multiple factors and return a factor.
#'
#' Note that function does not inherit from \code{\link{c}} to not change R semantics behind your back when this
#' package is loaded.
#'
#' @param ... [\code{factor}]\cr
#'   The factors.
#' @return [\code{factor}].
#' @export
#' @examples
#' f1 <- factor(c("a", "b"))
#' f2 <- factor(c("b", "c"))
#' print(c(f1, f2))
#' print(cFactor(f1, f2))
cFactor = function(...) {
  args = list(...)
  for (i in seq_along(args))
    if (!is.factor(args[[i]]))
      args[[i]] = as.factor(args[[i]])
  ## The first must be factor otherwise we wouldn't be inside
  ## c.factor, its checked anyway in the line above.
  newlevels = sort(unique(unlist(lapply(args, levels))))
  ans = unlist(lapply(args, function(x) {
        m = match(levels(x), newlevels)
        m[as.integer(x)]
      }))
  levels(ans) = newlevels
  class(ans) = "factor"
  return(ans)
}
