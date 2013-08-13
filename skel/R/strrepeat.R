#' Repeat and join a string
#'
#' @param x [character]\cr
#'   Vector of characters.
#' @param n [\code{integer(1)}]\cr
#'   Times the vector \code{x} is repeated.
#' @param sep [\code{character(1)}]\cr
#'   Separator to use to collapse the vector of characters.
#' @return \code{character(1)}.
#' @export
#' @examples
#' strrepeat("x", 3)
strrepeat = function(x, n, sep = "") {
  collapse(rep.int(x, max(0L, n)), sep)
}
