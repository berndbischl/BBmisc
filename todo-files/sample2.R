#' Samples a random element.
#'
#' Similar to \code{\link[base]{sample}}, but with the
#' small exception, that giving a single value x as input is treated as a single vector
#' and not like 1:x.
#' @param x [\code{vector}]\cr
#'   Source vector.
#' @param size [\code{integer(1)}]\cr
#'   Number of random elements to draw.
#' @param replace [\code{logical(1)}]\cr
#'   Draw random elements with or without replacement?
#'   Default is \code{TRUE}.
#' @return [\code{vector}]
#' @export
sample2 = function(x, size = 1L, replace = FALSE) {
  checkArg(size, cl = "integer", lower = 1L, len = 1L, na.ok = FALSE)
  checkArg(replace, cl ="logical", len = 1L, na.ok = FALSE)
  n = length(x)
  if (n == 0)
    stop("Given vector must not be empty.")
  # handling vectors of length one differs from the behavior of base::sample
  if (n == 1L) {
    if (!replace) {
      if (size > 1L) {
        stop("Cannot sample more than one element without replacement.")
      }
      return(x)
    } else {
      return(rep(x, size))
    }
  }
  return(sample(x, size = size, replace = replace))
}
