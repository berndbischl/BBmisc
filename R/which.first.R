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
#' @examples
#'  which.first(c(FALSE, TRUE))
#'  which.last(c(FALSE, FALSE))
which.first = function(x, use.names = TRUE) {
  wf(x, use.names)
}

#' @rdname which.first
#' @export
which.last = function(x, use.names = TRUE) {
  wl(x, use.names)
}
