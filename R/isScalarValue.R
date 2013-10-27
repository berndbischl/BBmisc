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
#' @export
isScalarValue = function(x, na.ok=TRUE, null.ok=FALSE) {
  if (is.null(x))
    return(null.ok)
  is.atomic(x) && length(x) == 1L && (na.ok || !is.na(x))
}
