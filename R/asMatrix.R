#' Extracts a named element from a list of lists.
#'
#' @param xs [\code{list}]\cr
#'   A list of vectors of the same length.
#' @param row.names [\code{character} | \code{integer}]\cr
#'   Row names of result.
#'   Default is to take the names of the elements of \code{xs}.
#' @param col.names [\code{character} | \code{integer}]\cr
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

  m = unique(viapply(xs, length))
  if (length(m) != 1L)
    stopf("Vectors must all be of the same length!")

  if (missing(row.names)) {
    row.names = names(xs[[1L]])
  } else {
    row.names = convertIntegers(row.names)
    checkArg(row.names, c("character", "integer"), len = m)
  }

  if (missing(col.names)) {
    col.names = names(xs)
  } else {
    col.names= convertIntegers(col.names)
    checkArg(col.names, c("character", "integer"), len = n)
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
