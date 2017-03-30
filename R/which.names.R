#' Find the names of the elements of a logical vector that are \code{TRUE}.
#'
#' @param x [\code{logical}]\cr
#'   Unique named logical vector without \code{NAs}.
#' @return [\code{character}]
#'   Returns the names of the elements in \code{x} that are \code{TRUE}.
#' @export
#' @examples
#'  which.names(c(a = FALSE, b = TRUE))
which.names = function(x) {
  assertLogical(x, any.missing = FALSE, names = "unique")
  names(x)[x]
}
