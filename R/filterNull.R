#' Filter a list for NULL values
#'
#' @param li [\code{list}]\cr
#'  List.
#' @return [\code{list}].
filterNull = function(li) {
  assertList(li)
  li[!vlapply(li, is.null)]
}
