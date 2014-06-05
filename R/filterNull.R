#' Filter a list for NULL values
#'
#' @param li [\code{list}]\cr
#'  List.
#' @return [\code{list}].
filterNull = function(li) {
  checkArg(li, "list")
  li[!vlapply(li, is.null)]
}
