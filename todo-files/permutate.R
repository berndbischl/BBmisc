#' Simple wrapper function for sample.
#'
#' One more function call necessary, but as a result the code
#' is much more read friendly.
#'
#' @param x [\code{vector}]\cr
#'   Source vector.
#' @return [\code{vector}]
#'   Source vector with permutated elements.
#' @export
permutate = function(x) {
  return(getRandomElements(x, size = length(x)))
}
