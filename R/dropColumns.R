#' Drop named columns of data frames or matricies
#'
#' @param x [\code{matrix} or \code{data.frame}]\cr
#'   Matrix or data frame for which columns should be removed.
#' @param cols [\code{character}]\cr
#'   Index specifing the columns to remove.
#' @return [\code{matrix} or \code{data.frame}]: Matrix or data frame with
#'   dropped columns.
#' @export
#' @examples
#' dropColumns(iris, "Species")
dropColumns = function(x, cols, drop.dim=FALSE) {
  checkArg(x, c("matrix", "data.frame"))
  checkArg(cols, "character", na.ok=FALSE)

  x[, setdiff(colnames(x), cols), drop=isTRUE(drop.dim)]
}
