#' Check if value is in between a range.
#'
#' @param x [\code{numeric(m)}]\cr
#'   Value(s) that should be within the range of \code{y}.
#' @param y [\code{numeric(n)}]\cr
#'   Numeric vector which defines the range
#' @usage x \%btwn\% y
#' @rdname btwn
#' @examples
#' x = 3
#' y = c(-1,2,5)
#' x %btwn% y
#' @export
`%btwn%` = function(x, y) {
  t = range(y)
  x <= t[2] & x >= t[1]
}

