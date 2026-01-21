#' @title A wrapper for \code{identical(x, FALSE)}
#'
#' @description
#' Tests if an object is identical to FALSE.
#'
#' @param x [any]\cr
#'   Your object.
#' @return [\code{logical(1)}].
#' @export
#' @examples
#' isFALSE(0)
#' isFALSE(FALSE)
isFALSE = function(x) {
  identical(x, FALSE)
}
