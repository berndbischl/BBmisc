#' Find the index of first/last \code{TRUE} value in a logical vector.
#'
#' @param x [\code{logical}]\cr
#'   Logical vector.
#' @return Returns a scalar integer of the index of the first/last \code{TRUE}
#'   value in \code{x} or an empty integer vector if none is found.
#' @export
#' @useDynLib BBmisc c_first
first = function(x, na.omit=TRUE) {
  .Call(c_first, x, na.omit, package="BBmisc")
}

#' @rdname first
#' @export
#' @useDynLib BBmisc c_last
last = function(x, na.omit=TRUE) {
  .Call(c_last, x, na.omit, package="BBmisc")
}
