#' Set a list element to a new value.
#'
#' This wrapper supports setting elements to \code{NULL}.
#'
#' @param obj [\code{list}]\cr
#' @param index [\code{character} | \code{integer}]\cr
#'   Index or indices where to insert the new values.
#' @param newval [any]\cr
#'   Inserted elements(s). 
#'   Has to be a list if \code{index} is a vector.
#' @return [\code{list}]
#' @export
setValue = function(obj, index, newval) {
  checkArg(obj, "list")
  index = convertIntegers(index)
  checkArg(index, cl = c("integer","character"))
  if (length(index) == 1L) {
    if (is.null(newval))
      obj[index] = list(NULL)
    else
      obj[index] = newval
  } else {
    checkArg(newval, cl = "list", len = length(index))
    obj[index] = newval
  }
  return(obj)
}

