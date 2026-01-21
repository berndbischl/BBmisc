#' @title Simply a negated \code{in} operator
#'
#' @description
#' Tests if elements of x are not in y.
#'
#' @param x [\code{vector}]\cr
#'   Values that should not be in \code{y}.
#' @param y [\code{vector}]\cr
#'   Values to match against.
#' @usage x \%nin\% y
#' @rdname nin
#' @export
`%nin%` = function(x, y) {
  !match(x, y, nomatch = 0L)
}
