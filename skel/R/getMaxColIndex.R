#' Find row- or columnwise the index of the maximal / minimal element in a matrix.
#'
#' @param x [\code{matrix(n,m)}] \cr
#'   Numerical input matrix.
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
#' @useDynLib BBmisc c_getMaxColIndex c_getMaxRowIndex
#' @examples
#' x <- matrix(runif(5 * 3), ncol=3)
#' print(x)
#' print(getMaxColIndex(x))
#' print(getMinColIndex(x))
getMaxColIndex = function(x, ties.method="random", na.rm=FALSE) {
  mode(x) = "numeric"
  ties.method = switch(ties.method, first=2, last=3, 1)
  .Call(c_getMaxColIndex, x, ties.method, na.rm)
}

#' @export
#' @rdname getMaxColIndex
getMinColIndex = function(x, ties.method="random", na.rm=FALSE) {
  getMaxColIndex(-x, ties.method, na.rm)
}

#' @export
#' @rdname getMaxColIndex
getMaxRowIndex = function(x, ties.method="random", na.rm=FALSE) {
  mode(x) = "numeric"
  ties.method = switch(ties.method, first=2, last=3, 1)
  .Call(c_getMaxRowIndex, x, ties.method, na.rm)
}

#' @export
#' @rdname getMaxColIndex
getMinRowIndex = function(x, ties.method="random", na.rm=FALSE) {
  getMaxRowIndex(-x, ties.method, na.rm)
}
