#' Find row- or columnwise the index of the maximal / minimal element in a matrix.
#'
#' \code{getMaxIndexOfRows} returns the index of the maximal element of each row.
#' \code{getMinIndexOfRows} returns the index of the minimal element of each row.
#' \code{getMaxIndexOfCols} returns the index of the maximal element of each col.
#' \code{getMinIndexOfCols} returns the index of the minimal element of each col.
#' If a corresponding vector (row or col) is empty, possibly after NA removal, -1 is returned
#' as index.
#'
#' @param x [\code{matrix(n,m)}] \cr
#'   Numerical input matrix.
#' @param weights [\code{numeric}]\cr
#'   Weights (same length as number of rows/cols).
#'   If these are specified, the index is selected from the weighted elements
#'   (see \code{\link{getMaxIndex}}).
#'   Default is \code{NULL} which means no weights.
#' @param ties.method [\code{character(1)}]\cr
#'   How should ties be handled?
#'   Possible are: \dQuote{random}, \dQuote{first}, \dQuote{last}.
#'   Default is \dQuote{random}.
#' @param na.rm [\code{logical(1)}]\cr
#'   If \code{FALSE}, NA is returned if an NA is encountered in \code{x}.
#'   If \code{TRUE}, NAs are disregarded.
#'   Default is \code{FALSE}
#' @return [\code{integer(n)}].
#' @export
#' @useDynLib BBmisc c_getMaxIndexOfRows c_getMaxIndexOfCols
#' @examples
#' x = matrix(runif(5 * 3), ncol = 3)
#' print(x)
#' print(getMaxIndexOfRows(x))
#' print(getMinIndexOfRows(x))
getMaxIndexOfRows = function(x, weights = NULL, ties.method = "random", na.rm = FALSE) {
  if (!is.double(x)) mode(x) = "numeric"
  ties.method = switch(ties.method, random = 1L, first = 2L, last = 3L,
                       stop("Unknown ties method"))
  assertFlag(na.rm)
  assertNumeric(weights, null.ok = TRUE, len = ncol(x))
  .Call(c_getMaxIndexOfRows, x, as.numeric(weights), ties.method, na.rm, PACKAGE = "BBmisc")
}

#' @export
#' @rdname getMaxIndexOfRows
getMinIndexOfRows = function(x, weights = NULL, ties.method = "random", na.rm = FALSE) {
  getMaxIndexOfRows(-x, weights, ties.method, na.rm)
}

#' @export
#' @rdname getMaxIndexOfRows
getMaxIndexOfCols = function(x, weights = NULL, ties.method = "random", na.rm = FALSE) {
  if (!is.double(x)) mode(x) = "numeric"
  ties.method = switch(ties.method, random = 1L, first = 2L, last = 3L,
                       stop("Unknown ties method"))
  assertFlag(na.rm)
  assertNumeric(weights, null.ok = TRUE, len = nrow(x))
  .Call(c_getMaxIndexOfCols, x, as.numeric(weights), ties.method, na.rm, PACKAGE = "BBmisc")
}

#' @export
#' @rdname getMaxIndexOfRows
getMinIndexOfCols = function(x, weights = NULL, ties.method = "random", na.rm = FALSE) {
  getMaxIndexOfCols(-x, weights, ties.method, na.rm)
}
