#' Convert rows (columns) of data.frame or matrix to lists.
#'
#' For each row, one list/vector is constructed, each entry of
#' the row becomes a list/vector element.
#'
#' @param x [\code{matrix} | \code{data.frame}]\cr
#'   Object to convert.
#' @param name.list [\code{logical(1)}]\cr
#'   Name resulting list with names of rows (cols) of \code{x}?
#'   Default is \code{FALSE}.
#' @param name.vector [\code{logical(1)}]\cr
#'   Name vector elements in resulting list with names of cols (rows) of \code{x}?
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
convertRowsToList = function(x, name.list = TRUE, name.vector = FALSE,
  factors.as.char = TRUE, as.vector = TRUE) {

  checkArg(x, c("matrix", "data.frame"))
  checkArg(name.list, "logical", len = 1L, na.ok = FALSE)
  checkArg(name.vector, "logical", len = 1L, na.ok = FALSE)
  checkArg(factors.as.char, "logical", len = 1L, na.ok = FALSE)
  checkArg(as.vector, "logical", len = 1L, na.ok = FALSE)
  ns.list = if (name.list) colnames(x) else NULL
  ns.vector = if (name.vector) rownames(x) else NULL
  if (is.matrix(x)) {
    if (as.vector)
      res = lapply(seq_row(x), function(i) setNames(x[i, ], ns.vector))
    else
      res = lapply(seq_row(x), function(i) setNames(as.list(x[i, ]), ns.vector))
  } else if (is.data.frame(x)) {
    if (factors.as.char)
      x = convertDataFrameCols(x, factors.as.char = TRUE)
    res = rowLapply(x, function(row) setNames(as.list(row), ns.vector))
  }
  setNames(res, ns.list)
}

#' @rdname convertRowsToList
#' @export
convertColsToList = function(x, name.list = FALSE, name.vector= FALSE,
  factors.as.char = TRUE, as.vector = TRUE) {

  convertRowsToList(t(x), name.list = name.list, name.vector = name.vector,
    factors.as.char = factors.as.char, as.vector = as.vector)
}



