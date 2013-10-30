#' Shortens strings to a given length.
#'
#' @param x [\code{character}]\cr
#'   Vector of strings.
#' @param len [\code{integer(1)}]\cr
#'   Absolute length the string should be clipped to, including \code{tail}.
#'   Note that you cannot clip to a shorter length than \code{tail}.
#' @param tail [\code{character(1)}]\cr
#'   If the string has to be shortened at least 1 character, the final characters will be \code{tail}.
#'   Default is \dQuote{...}.
#' @return [\code{character(1)}].
#' @export
#' @examples
#' print(clipString("abcdef", 10))
#' print(clipString("abcdef", 5))
clipString = function(x, len, tail="...") {
  checkArg(x, "character", na.ok=TRUE)
  len = convertInteger(len)
  checkArg(tail, "character", len=1L, na.ok=FALSE)
  checkArg(len, "integer", len=1L, na.ok=FALSE, lower=nchar(tail))
  #if (is.na(x))
  #  return(NA_character_)
  ind = !is.na(x) & nchar(x) > len
  replace(x, ind, paste(substr(x[ind], 1L, len - nchar(tail)), tail, sep=""))
}
