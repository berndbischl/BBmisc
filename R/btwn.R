#' Check if value is within a range.
#'
#' @param x [\code{numeric(m)}]\cr
#'   Value(s) that should be within the range of \code{y}.
#' @param y [\code{numeric(n)}]\cr
#'   Numeric vector which defines the range
#' @usage x \%within\% y
#' @rdname within
#' @examples
#' x = 3
#' y = c(-1,2,5)
#' x %within% y
#' @export
`%within%` = function(x, y) {
  t = range(y)
  x <= t[2] & x >= t[1]
}

