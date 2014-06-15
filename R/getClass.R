#' Wrapper for \code{class(x)[1]}.
#'
#' @param x [any]\cr
#'   Input object.
#' @return [\code{character(1)}].
#' @export
getClass = function(x) {
  # FIXME: overloads getClass from methods!
  class(x)[1L]
}
