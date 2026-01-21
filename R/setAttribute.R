#' @title A wrapper for \code{attr(x, which) = y}
#'
#' @description
#' Sets an attribute on an object and returns the modified object.
#'
#' @param x [any]\cr
#'  Your object.
#' @param which [\code{character(1)}]\cr
#'  Name of the attribute to set
#' @param value [\code{ANY}]\cr
#'  Value for the attribute.
#' @return Changed object \code{x}.
#' @export
#' @examples
#' setAttribute(list(), "foo", 1)
setAttribute = function(x, which, value) {
  attr(x, which) = value
  x
}
