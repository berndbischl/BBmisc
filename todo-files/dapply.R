#' Apply function and coerce to a data frame.
#'
#' Performs an \code{\link[base]{apply}} on columns if input is a
#' matrix and calls \code{\link[base]{lapply}} otherwise.
#' The respective result is then coerced to a data frame.
#'
#' @param x [\code{ANY}]\cr
#'   Object to apply function on.
#' @param fun [function]\cr
#'   Function to apply.
#' @param ... [\code{ANY}]\cr
#'   Additional arguments for \code{fun}.
#' @param stringsAsFactors [\code{logical(1)}]\cr
#'   Convert strings to factors?
#'   See \code{\link[base]{default.stringsAsFactors}} which is also
#'   the default.
dapply = function(x, fun, ..., stringsAsFactors=default.stringsAsFactors()) {
  x = if(is.matrix(x)) apply(x, 2L, fun, ...) else lapply(x, fun, ...)
  as.data.frame(x, stringsAsFactors=stringsAsFactors)
}
