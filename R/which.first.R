#' Find the index of first/last \code{TRUE} value in a logical vector.
#'
#' @param x [\code{logical}]\cr
#'   Logical vector.
#' @param use.names [\code{logical(1)}]\cr
#'   If \code{TRUE} and \code{x} is named, the result is also
#'   named.
#' @return [\code{integer(1)} | \code{integer(0)}].
#'   Returns the index of the first/last \code{TRUE} value in \code{x} or
#'   an empty integer vector if none is found.
#' @export
which.first = function(x, use.names = TRUE) {
  if (isTRUE(use.names))
    head(which(x), 1L)
  else
    head(unname(which(x)), 1L)
}

#' @rdname which.first
#' @export
which.last = function(x, use.names = TRUE) {
  if (isTRUE(use.names))
    tail(which(x), 1L)
  else
    tail(unname(which(x)), 1L)
}
