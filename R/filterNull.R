#' @title Filter a list for NULL values
#'
#' @description
#' Removes all NULL elements from a list.
#'
#' @param li [\code{list}]\cr
#'  List.
#' @return [\code{list}].
#' @export
filterNull = function(li) {
  assertList(li)
  li[!vlapply(li, is.null)]
}
