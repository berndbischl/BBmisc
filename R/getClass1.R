#' Wrapper for \code{class(x)[1]}.
#'
#' @param x [any]\cr
#'   Input object.
#' @return [\code{character(1)}].
#' @note \code{getClass} is a function in \code{methods}. Do not confuse.
#' @export
<<<<<<< HEAD:R/getClass.R
getClass = function(x) {
  # FIXME: overloads getClass from methods!
||||||| merged common ancestors
getClass = function(x) {
=======
getClass1 = function(x) {
>>>>>>> master:R/getClass1.R
  class(x)[1L]
}
