#' Determines used factor levels.
#'
#' Determines the factor levels of a factor type vector
#' that are actually occuring in it.
#'
#' @param x [\code{factor}]\cr
#'   Source vector.
#' @return [\code{character}]
#'   Character vector of used levels.
#' @export
getUsedFactorLevels = function(x) {
  stopifnot(is.factor(x))
  return(intersect(levels(x), unique(x)))
}
