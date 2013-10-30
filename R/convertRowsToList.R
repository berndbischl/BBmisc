#' Convert rows of data.frame or matrix to lists.
#'
#' For each row, one list is constructed, each entry of
#' the row becomes a separate list element.
#'
#' @param x [\code{matrix} | \code{data.frame}]\cr
#'   Object to convert.
#' @param use.names [\code{logical(1)}]\cr
#'   Name elements of lists with column names of \code{x}.
#'   Default is \code{FALSE}.
#' @param factors.as.char [\code{logical(1)}]\cr
#'   If \code{x} is a data.frame, convert factor columns to 
#'   string elements in the resulting lists?
#'   Default is \code{TRUE}.
#' @return [\code{list} of lists].
#' @export 
convertRowsToList = function(x, use.names=FALSE, factors.as.char=TRUE) {
  checkArg(x, c("matrix", "data.frame"))
  checkArg(use.names, "logical", len=1L, na.ok=FALSE) 
  ns = if (use.names) colnames(x) else NULL
  if (is.matrix(x)) {
    apply(x, 1, function(row) setNames(as.list(row), ns))
  } else if (is.data.frame(x)) {
    if (factors.as.char)
      x = convertDataFrameCols(x, factors.as.char=TRUE)
    rowLapply(x, function(row) setNames(as.list(row), ns))
  }
}
