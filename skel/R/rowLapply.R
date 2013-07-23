#' Apply funciton to rows of a data frame
#'
#' Just like an \code{\link[base]{lapply}} on data frames,
#' but on the rows.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame.
#' @param fun [\code{function}]\cr
#'   Function to apply. Rows are passed as list as first unnamed argument.
#' @param ... [\code{ANY}]\cr
#'   Additional arguments for \code{fun}.
#' @return \code{list} of length \code{nrow(df)}.
#' @export
#' @examples
#'  rowLapply(iris, function(x) x$Sepal.Length + x$Sepal.Width)
rowLapply = function(df, fun, ...) {
  checkArg(df, "data.frame")
  fun = match.fun(fun)
  .wrap = function(.i, .df, .fun, ...)
    .fun(as.list(.df[.i, ], ...))
  lapply(seq_len(nrow(df)), .wrap, .fun = fun, .df = df, ...)
}
