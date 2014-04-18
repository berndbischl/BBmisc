#' Calculate range statistic.
#'
#' A simple wrapper for \code{diff(range(x))}, so \code{max(x) - min(x)}.
#'
#' @param x [\code{numeric}]\cr
#'   The vector.
#' @param na.rm [\code{logical(1)}]\cr
#'   If \code{FALSE}, NA is returned if an NA is encountered in \code{x}.
#'   If \code{TRUE}, NAs are disregarded.
#'   Default is \code{FALSE}
#' @return [\code{numeric(1)}].
#' @export
rangeVal = function(x, na.rm = FALSE) {
  checkArg(x, "numeric", min.len = 1L, na.ok = TRUE)
  checkArg(na.rm, "logical", len = 1L, na.ok = FALSE)
  if (all(is.na(x)))
    return(NA_real_)
  diff(range(x, na.rm = na.rm))
}


