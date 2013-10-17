#' Apply function to rows of a data frame.
#'
#' Just like an \code{\link[base]{lapply}} on data frames,
#' but on the rows.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame.
#' @param fun [\code{function}]\cr
#'   Function to apply. Rows are passed as list or vector,
#'   depending on argument \code{unlist}, as first argument.
#' @param ... [\code{ANY}]\cr
#'   Additional arguments for \code{fun}.
#' @param unlist [\code{logical(1)}]\cr
#'   Unlist the row? Note that automatic conversion may be triggered for
#'   lists of mixed data types
#'   Default is \code{FALSE}.
#' @param simplify [\code{logical(1)}]\cr
#'   Should the result be simplified?
#'   See \code{\link{sapply}}.
#'   Default is \code{TRUE}.
#' @param use.names [\code{logical(1)}]\cr
#'   Should result be named by the row names of \code{df}?
#'   Default is \code{TRUE}.
#' @return [\code{list} or simplified object]. Length is \code{nrow(df)}.
#' @export
#' @examples
#'  rowLapply(iris, function(x) x$Sepal.Length + x$Sepal.Width)
rowLapply = function(df, fun, ..., unlist = FALSE) {
  checkArg(df, "data.frame")
  fun = match.fun(fun)
  checkArg(unlist, "logical", len=1L, na.ok=FALSE)
  if(unlist) {
    .wrap = function(.i, .df, .fun, ...)
      .fun(unlist(.df[.i, ], recursive=FALSE, use.names=TRUE), ...)
  } else {
    .wrap = function(.i, .df, .fun, ...)
      .fun(as.list(.df[.i, ], ...))
  }

  lapply(seq_len(nrow(df)), .wrap, .fun = fun, .df = df, ...)
}

#' @export
#' @rdname rowLapply
rowSapply = function(df, fun, ..., unlist = FALSE, simplify=TRUE, use.names=TRUE) {
  checkArg(simplify, "logical", len=1L, na.ok=FALSE)
  checkArg(use.names, "logical", len=1L, na.ok=FALSE)
  ys = rowLapply(df, fun, ..., unlist = FALSE)
  if (simplify && length(ys)) 
    ys = simplify2array(ys)
  if (use.names) {
    if (is.matrix(ys)) {
      colnames(ys) = rownames(df)
      rownames(ys) = NULL
    } else {
      names(ys) = rownames(df)
    }
  } else {
    names(ys) = NULL
  }
  return(ys)
}


