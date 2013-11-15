#' Drop named elements of an object
#'
#' @param x\cr
#'   Object to drop named elements from.
#'   For matrix and data frames this function delegates to drop
#'   using the second dimension (columns).
#'   The normal index operator \dQuote{[} is applied otherwise otherwise.
#' @param drop [\code{character}]\cr
#'   Names to drop.
#' @return Subset of object of same type as \code{x}.
#' @export
dropNamed = function(x, drop) {
  checkArg(drop, "character", na.ok=FALSE)

  if (is.matrix(x) || is.data.frame(x))
    x[, setdiff(colnames(x), drop), drop=FALSE]
  else
    x[setdiff(names(x), drop)]
}
