#' Is given argument an atomic vector or factor of length 1?
#'
#' @param x [any]\cr
#'   Argument.
#' @return [\code{logical(1)}].
isScalarValue = function(x) {
  is.atomic(x) && length(x) == 1L
}