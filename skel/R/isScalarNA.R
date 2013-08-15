#' Checks whether an object is a scalar NA value.
#'
#' Checks whether object is from \code{(NA, NA_integer, NA_real_, NA_character_, NA_complex_)}.
#' @param x [any]\cr
#'   Object to check.
#' @return [\code{logical(1)}].
#' @export
isScalarNA = function(x) {
  identical(x, NA) || 
  identical(x, NA_integer_) || 
  identical(x, NA_real_) || 
  identical(x, NA_character_) || 
  identical(x, NA_complex_)
}