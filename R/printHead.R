#' More meaningful \code{head(df)} output.
#'
#' The behaviour is similar to \code{print(head(x, n))}. The difference is, that if
#' the number of rows in a data.frame/matrix or the number of elements in a list
#' or vector is larger than \code{n}, additional information is printed about
#' the total number of rows or elements respectively.
#'
#' @param x [\code{data.frame} | \code{matrix}]\cr
#'   Data frame or matrix.
#' @param n \code{integer(1)}\cr
#'   Single positive integer: number of rows for a matrix/data.frame or number of
#'   elements for vectors/lists respectively.
printHead = function(x, n = 6L) {
  assertCount(n, positive = TRUE)
  print(head(x, n = n))
  if ((is.data.frame(x) || is.matrix(x)) && nrow(x) > n)
    catf("... (#rows: %i, #cols: %i)", nrow(x), ncol(x))
  else if (length(x) > n)
    catf("... (#elements: %i)", length(x))
  invisible(NULL)
}
