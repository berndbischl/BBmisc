#' Wrapper for \code{class(x)[1L]}.
#'
#' @param x [any]\cr
#'   Input object.
#' @return [\code{character(1)}].
#' @export
getClass = function(x) {
  class(x)[1L]
}

