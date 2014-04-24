#' Check if given object has certain attributes.
#'
#' @param obj [\code{mixed}]\cr
#'   Arbitrary R object.
#' @param attributeNames [\code{character}]\cr
#'   Vector of strings, i.e., attribute names.
#' @return [\code{logical(1)}]
#'   \code{TRUE} if object \code{x} contains all attributes from \code{attributeNames}
#'   and \code{FALSE} otherwise.
#' @export
hasAttributes = function(obj, attributeNames) {
  return(isSubset(attributeNames, getAttributeNames(obj)))
}
