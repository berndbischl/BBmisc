
#' @title A wrapper to add to the class attribute
#'
#' @description
#' Adds classes to the class attribute of an object.
#'
#' @param x [any]\cr
#'   Your object.
#' @param classes [\code{character}]\cr
#'  Classes to add. Will be added in front (specialization).
#' @return Changed object \code{x}.
#' @export
#' @examples
#' x = list()
#' print(class(x))
#' x = addClasses(x, c("foo1", "foo2"))
#' print(class(x))
addClasses = function(x, classes) {
  class(x) = c(classes, class(x))
  x
}
