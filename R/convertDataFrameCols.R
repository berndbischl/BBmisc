#' Converts columns in a data frame to characters, factors or numerics.
#' 
#' \code{convertDfCols} is deprecated.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame.
#' @param chars.as.factor [\code{logical(1)}]\cr
#'   Should characters be converted to factors?
#'   Default is \code{FALSE}.
#' @param factors.as.char [\code{logical(1)}]\cr
#'   Should characters be converted to factors?
#'   Default is \code{FALSE}.
#' @param ints.as.num [\code{logical(1)}]\cr
#'   Should integers be converted to numerics?
#'   Default is \code{FALSE}.
#' @param logicals.as.factor [\code{logical(1)}]\cr
#'   Should logicals be converted to factors?
#'   Default is \code{FALSE}.
#' @export
#' @return [\code{data.frame}].
convertDataFrameCols = function(df, chars.as.factor = FALSE, factors.as.char = FALSE, ints.as.num = FALSE, logicals.as.factor=FALSE) {
  checkArg(df, "data.frame")
  checkArg(chars.as.factor, "logical", len=1L, na.ok=FALSE)
  checkArg(factors.as.char, "logical", len=1L, na.ok=FALSE)
  checkArg(ints.as.num, "logical", len=1L, na.ok=FALSE)
  checkArg(logicals.as.factor, "logical", len=1L, na.ok=FALSE)
  df = x = as.list(df)

  if (chars.as.factor) {
    i = vapply(df, is.character, TRUE)
    if (any(i))
      x[i] = lapply(x[i], factor)
  }

  if (factors.as.char) {
    i = vapply(df, is.factor, TRUE)
    if (any(i))
      x[i] = lapply(x[i], as.character)
  }

  if (ints.as.num) {
    i = vapply(df, is.integer, TRUE)
    if (any(i))
      x[i] = lapply(x[i], as.double)
  }

  if (logicals.as.factor) {
    i = vapply(df, is.logical, TRUE)
    if (any(i))
      x[i] = lapply(x[i], factor, levels=c("TRUE", "FALSE"))
  }

  as.data.frame(x, stringsAsFactors=FALSE)
}

