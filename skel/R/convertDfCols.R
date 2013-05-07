#' Converts all character columns in a data.frame to factors
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame-
#' @param chars.as.factor [\code{logical(1)}]\cr
#'   Should characters be converted to factors?
#'   Default is \code{FALSE}.
#' @param factors.as.char [\code{logical(1)}]\cr
#'   Should characters be converted to factors?
#'   Default is \code{FALSE}.
#' @param ints.as.num [\code{logical(1)}]\cr
#'   Should integers be converted to numerics?
#'   Default is \code{FALSE}.
#' @export
#' @return [\code{data.frame}].
convertDfCols = function(df, chars.as.factor = FALSE, factors.as.char = FALSE, ints.as.num = FALSE) {
  #FIXME: speed in general can be increased
  checkArg(df, "data.frame")
  checkArg(chars.as.factor, "logical", len=1L, na.ok=FALSE)
  checkArg(factors.as.char, "logical", len=1L, na.ok=FALSE)
  checkArg(ints.as.num, "logical", len=1L, na.ok=FALSE)
  df2 = df
  if (chars.as.factor) {
    inds = which(sapply(df, is.character))
    if (length(inds) > 0) {
      for (i in inds)
        df2[, i] = factor(df[, i])
    }
  }
  if (factors.as.char) {
    inds = which(sapply(df, is.factor))
    if (length(inds) > 0) {
      for (i in inds)
        df2[, i] = as.character(df[, i])
    }
  }
  if (ints.as.num) {
    inds = which(sapply(df, is.integer))
    if (length(inds) > 0) {
      for (i in inds)
        df2[, i] = as.numeric(df[, i])
    }
  }
  return(df2)
}
