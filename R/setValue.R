#' Set a list element to a new Value. This wrapper supports setting elements to \code{NULL}.
#'
#' @param obj [\code{list}]\cr
#' @param index [\code{character} | \code{integer}]\cr
#'   Index where to put the new Value(s).
#' @param newVal [mixed]
#'   Will be wrapped in a new list element. Has to be a list if \code{index} is a vector.
#' @return [\code{list}]
#' @export
setValue = function(obj, index, newVal){
  index = convertIntegers(index)
  checkArg(obj, cl="list")
  if(length(index)>1)
    checkArg(newVal, cl="list")
  if(!is.null(newVal))
    checkArg(index, cl=c("integer","character"), len=length(newVal))
  if(!is.list(newVal))
    newVal = list(newVal)
  names(newVal) = index
  obj[index] = newVal
  obj
}

