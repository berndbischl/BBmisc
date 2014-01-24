#' DEPRECATED
#'
#' Use \code{\link{convertDataFrameCols}} instead.
#' @keywords internal
#' @export
convertDfCols = function(...) {
  convertDataFrameCols(...)
}

#' DEPRECATED
#'
#' Use \code{\link{listToShortString}} instead.
#' @keywords internal
#' @export
listToShortString = function(...) {
  convertToShortString(...)
}

#' DEPRECATED
#'
#' Use \code{\link{convertDataFrameCols}} instead.
#' @keywords internal
#' @export
stringsAsFactors = function(x, ...) {
  convertDataFrameCols(x, chars.as.factor=TRUE, ...)
}
