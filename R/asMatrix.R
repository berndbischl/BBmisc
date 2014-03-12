#' Extracts a named element from a list of lists.
#'
#' @param xs [\code{list}]\cr
#'   A list of vectors of the same length.
#' @param row.names [\code{character(1)}]\cr
#'   Row names of result.
#'   Default is to take the names of the elements of \code{xs}.
#' @param col.names [\code{character(1)}]\cr
#'   Column names of result.
#'   Default is to take the names of the elements of \code{xs}.
#' @return [\code{matrix}].
#' @export
asMatrixCols = function(xs, row.names, col.names) {
  checkArg(xs, "list")
  n = length(xs)
  if (n == 0L)
    return(matrix(0, nrow = 0L, ncol = 0L))
  checkListElementClass(xs, "vector")
  lens = sapply(xs, length)
  if (length(unique(lens)) != 1L)
    stopf("Vectors must all be of the same length!")
  m = lens[[1]]
  if (missing(row.names)) {
    row.names = names(xs[[1]])
  } else {
    checkArg(row.names, "character", len = n)
  }
  if (missing(col.names)) {
    col.names = names(xs)
  } else {
    checkArg(col.names, "character", len = length(xs))
  }

  xs = unlist(xs)
  dim(xs) = c(m, n)
  rownames(xs) = row.names
  colnames(xs) = col.names
  return(xs)
}

#' @rdname asMatrixCols
#' @export
asMatrixRows = function(xs, row.names, col.names) {
  t(asMatrixCols(xs, row.names, col.names))
}
