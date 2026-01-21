#' @title Calculates symmetric set difference between two sets
#'
#' @description
#' Returns elements that are in either set but not in both.
#'
#' @param x [\code{vector}]\cr
#'   Set 1.
#' @param y [\code{vector}]\cr
#'   Set 2.
#' @return [\code{vector}].
#' @export
symdiff = function(x, y) {
  setdiff(union(x, y), intersect(x, y))
}
