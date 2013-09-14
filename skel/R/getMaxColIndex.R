#' Find row- or columnwise the index of the maximal / minimal element in a matrix.
#'
#' @param x [\code{matrix(n,m)}] \cr
#'   Numerical input matrix.
#' @param na.rm [\code{numeric}]\cr
#'   If \code{FALSE}, NA is returned if an NA is encountered in \code{x}.
#'   If \code{TRUE}, NAs are disregarded.
#'   Default is \code{FALSE}
#' @return [\code{integer(n)}].
#' @export
#' @useDynLib BBmisc c_getMaxColIndex c_getMaxRowIndex
#' @examples
#' x <- matrix(runif(5 * 3), ncol=3)
#' print(x)
#' print(getMaxColIndex(x))
#' print(getMinColIndex(x))
getMaxColIndex = function(x, na.rm=FALSE) {
  mode(x) = "numeric"
  .Call(c_getMaxColIndex, x, na.rm)
}

#' @export
#' @rdname getMaxColIndex
getMinColIndex = function(x, na.rm=FALSE) {
  getMaxColIndex(-x, na.rm)
}

#' @export
#' @rdname getMaxColIndex
getMaxRowIndex = function(x, na.rm=FALSE) {
  mode(x) = "numeric"
  .Call(c_getMaxRowIndex, x, na.rm)
}

#' @export
#' @rdname getMaxColIndex
getMinRowIndex = function(x, na.rm=FALSE) {
  getMaxRowIndex(-x, na.rm)
}
