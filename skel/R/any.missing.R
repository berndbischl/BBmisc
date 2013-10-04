#' Check if an object contains missing values.
#'
#' Supported are atomic types (see \code{\link[base]{is.atomic}}) and lists.
#' Missingness is defined as values being \code{NA} or \code{NaN} for atomic types and
#' \code{NULL} for lists.
#'
#' @param x [\code{ANY}]\cr
#'  Object to check. 
#' @return [\code{logical(1)}] Returns \code{TRUE} if at least one element of \code{x} was missing (see details).
#' @useDynLib BBmisc c_any_missing
#' @export
any.missing = function(x) {
  .Call("c_any_missing", x)
}
