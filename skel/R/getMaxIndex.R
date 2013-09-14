#' Return index of maximal/minimal element in numerical vector.
#' 
#' If \code{x} is empty or only contains NAs which are to be removed,
#' -1 is returned.
#' 
#' @param x [\code{numeric}]\cr
#'   Input vector.
#' @param na.rm [\code{numeric}]\cr
#'   If \code{FALSE}, NA is returned if an NA is encountered in \code{x}.
#'   If \code{TRUE}, NAs are disregarded.
#'   Default is \code{FALSE}
#' @return [\code{integer(1)}].   
#' @export
#' @useDynLib BBmisc c_getMaxIndex
getMaxIndex = function(x, ties.method="random", na.rm=FALSE) {
  .Call(c_getMaxIndex, as.numeric(x), ties.method, na.rm)
}


#' @export
#' @rdname getMaxIndex
getMinIndex = function(x, ties.method="random", na.rm=FALSE) {
  getMaxIndex(-as.numeric(x), ties.method, na.rm)
}
