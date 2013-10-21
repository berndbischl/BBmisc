#' Drop columns of data frames and matricies
#'
#' @param x [\code{matrix} or \code{data.frame}]\cr
#'   Matrix or data frame for which columns should be removed.
#' @param cols [\code{character}, \code{numeric} or \code{logical}]\cr
#'   Index specifing the columns to remove. The negation will be passed
#'   together with \code{drop=FALSE} to the index operator.
#' @return [\code{matrix} or \code{data.frame}]: Matrix or data frame with
#'   dropped columns.
#' @export
#' @examples
#' dropColumns(iris, "Species")
#' dropColumns(iris, 4)
#' dropColumns(iris, -(1:3))
dropColumns = function(x, cols) {
  checkArg(x, c("matrix", "data.frame"))

  if (is.character(cols)) {
    if (!isProperlyNamed(x))
      stop("Argument 'x' must be properly named")
    x[, setdiff(colnames(x), cols), drop=FALSE]
  } else if (is.numeric(cols)) {
    cols = convertInteger(cols)
    checkArg(cols, "integer", na.ok=FALSE)
    x[, -cols, drop=FALSE]
  } else if (is.logical(cols)) {
    checkArg(cols, "logical", len=ncol(x), na.ok=FALSE)
    x[, !cols, drop=FALSE]
  } else {
    stop("Argument 'cols' must be a character, integer or logical")
  }
}
