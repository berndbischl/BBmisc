#' Wrapper for message and sprintf.
#' 
#' A simple wrapper for \code{message(sprintf(...))}.
#' 
#' @param ... [any]\cr
#'   See \code{\link{sprintf}}.
#' @return Nothing.
#' @export
#' @examples
#' msg = "a message"
#' warningf("this is %s", msg)
messagef = function(...) {
  message(sprintf(...))
}
