#' Calculate range statistic.
#'
#' A simple wrapper for \code{diff(range(x))}, so \cod€{max(x) - min(x)}.
#'
#' @param x [\code{numeric}]\cr
#'   The vector.
#' @return [\code{numeric(1)}].
#' @export
rangeVal = function(x) {
  checkArg(x, "numeric", min.len = 1L, na.ok = TRUE)
  diff(range(x))
}
