#' Find rowise maximum index in a matrix
#'
#' Find the maximum position for each row of a matrix, breaking ties
#' at random.
#'
#' @param m [\code{numeric}] \cr
#'   numerical matrix.
#'
#' @return Integer vector of length \code{nrow(m)}.
#'
#' @examples
#' m <- matrix(runif(10000 * 3), ncol=3)
#' table(maxColumn(m))
#' 
#' @export
#' @useDynLib BBmisc do_randomized_numeric_maxColumn
maxColumn <- function(m) {
  checkArg(m, "matrix")
  checkArg(m, "numeric")
  .Call(do_randomized_numeric_maxColumn, m)
}
