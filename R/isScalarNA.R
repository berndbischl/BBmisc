#' Checks whether an object is a scalar NA value.
#'
#' Checks whether object is from \code{(NA, NA_integer, NA_real_, NA_character_, NA_complex_)}.
#' @param x [any]\cr
#'   Object to check.
#' @return [\code{logical(1)}].
#' @useDynLib BBmisc c_isScalarNA
#' @export
isScalarNA = function(x) {
  .Call("c_isScalarNA", x, PACKAGE="BBmisc")
}
