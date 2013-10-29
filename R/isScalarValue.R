#' Is given argument an atomic vector or factor of length 1?
#'
#' @param x [any]\cr
#'   Argument.
#' @param na.ok [\code{logical(1)}]\cr
#'   Is \code{NA} considered a scalar?
#'   Default is \code{TRUE}.
#' @param null.ok [\code{logical(1)}]\cr
#'   Is \code{NULL} considered a scalar?
#'   Default is \code{FALSE}.
#' @return [\code{logical(1)}].
#' @useDynLib BBmisc c_isScalarValue
#' @export
isScalarValue = function(x, na.ok=TRUE, null.ok=FALSE) {
  .Call("c_isScalarValue", x, na.ok, null.ok, PACKAGE="BBmisc")
}
