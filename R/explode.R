#' Split a string into substrings.
#'
#' Split up a string into substrings according to a seperator.
#'
#' @param x [\code{character}]\cr
#'   Source string.
#' @param sep [\code{character}]\cr
#'   Seperator whcih is used to split \code{x} into substrings.
#'   Default is \dQuote{ }.
#' @return [\code{vector}]
#'   Vector of substrings.
#' @export
explode = function(x, sep = " ") {
  checkArg(x, "character", na.ok = FALSE)
  checkArg(sep, "character", na.ok = FALSE)
  x.exploded = strsplit(x, sep, perl = TRUE)
  return(x.exploded[[1]])
}
