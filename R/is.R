#' Binary operator which acts as a wrapper for \code{\link[base]{inherits}}.
#'
#' @param object [\code{any}]\cr
#'   A R object.
#' @param class2 [\code{character(1)}]\cr
#'   A character vector naming the class.
#' @return [\code{logical(1)}]
#'   Returns \code{TRUE} if object is of the corresponding class and \code{FALSE} otherwise.
#' @rdname is
#' @export
`%is%` = function(object, class2) {
  checkArg(class2, "character", len = 1L, na.ok = FALSE)
  inherits(object, class2, which = FALSE)
}
