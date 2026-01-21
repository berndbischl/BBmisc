#' @title Helper function for determining the vector of attribute names of a given object
#'
#' @description
#' Returns the names of all attributes of an object.
#'
#' @param obj [any]\cr
#'   Source object.
#' @return [\code{character}]
#'   Vector of attribute names for the source object.
#' @export
getAttributeNames = function(obj) {
  names(attributes(obj))
}
