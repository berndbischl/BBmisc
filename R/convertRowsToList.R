#' Convert rows of data.frame or matrix to lists.
#'
#' For each row, one list/vector is constructed, each entry of
#' the row becomes a list/vector element.
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
#' @param as.vector [\code{logical(1)}]\cr
#'   If \code{x} is a matrix, store rows as vectors in the resulting list - or otherwise as lists?
#'   Default is \code{TRUE}.
#' @return [\code{list} of lists or vectors].
#' @export
convertRowsToList = function(x, use.names = FALSE, factors.as.char = TRUE, as.vector = TRUE) {
  checkArg(x, c("matrix", "data.frame"))
  checkArg(use.names, "logical", len = 1L, na.ok = FALSE)
  checkArg(factors.as.char, "logical", len = 1L, na.ok = FALSE)
  checkArg(as.vector, "logical", len = 1L, na.ok = FALSE)
  ns = if (use.names) colnames(x) else NULL
  if (is.matrix(x)) {
    if (as.vector)
      lapply(seq_row(x), function(i) setNames(x[i, ], ns))
    else
      lapply(seq_row(x), function(i) setNames(as.list(x[i, ]), ns))
  } else if (is.data.frame(x)) {
    if (factors.as.char)
      x = convertDataFrameCols(x, factors.as.char = TRUE)
    rowLapply(x, function(row) setNames(as.list(row), ns))
  }
}
