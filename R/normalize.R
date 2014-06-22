#' Normalizes numeric data to a given scale.
#'
#' Currently implemented for numeric vectors, numeric matrices and data.frame.
#' For matrixes one can operate on rows or columns
#' For data.frames, only the numeric columns are touched, all others are left unchanged.
#'
#' @param x [\code{numeric} | \code{matrix} | \code{data.frame}]\cr
#'   Input vector.
#' @param method [\code{character(1)}]\cr
#'   Normalizing method. Available are:\cr
#'   \dQuote{center}: Subtract mean.\cr
#'   \dQuote{scale}: Divide by standard deviation.\cr
#'   \dQuote{standardize}: Center and scale.\cr
#'   \dQuote{range}: Scale to a given range.\cr
#' @param range [\code{numeric(2)}]\cr
#'   Range for method \dQuote{range}.
#'   Default is \code{c(0,1)}.
#' @param margin [\code{integer(1)}]\cr
#'   1 = rows, 2 = cols.
#'   Same is in \code{\link{apply}}
#'   Default is 1.
#' @return [\code{numeric} | \code{matrix} | \code{data.frame}].
#' @seealso \code{\link{scale}}
#' @export
normalize = function(x, method = "standardize", range = c(0, 1), margin = 1L) {
  assertChoice(method, c("range", "standardize", "center", "scale"))
  assertNumeric(range, len = 2L, any.missing = FALSE)
  UseMethod("normalize")
}

#' @export
normalize.numeric = function(x, method = "standardize", range = c(0,1), margin = 1L) {
  normalize2(x, method, range)
}

#' @export
normalize.matrix = function(x, method = "standardize", range = c(0, 1), margin = 1L) {
  x = apply(x, margin, normalize2, method = method, range = range)
  if (margin == 1L)
    x = t(x)
  return(x)
}

#' @export
normalize.data.frame = function(x, method = "standardize", range = c(0, 1), margin = 1L) {
  isnum = sapply(x, is.numeric)
  if (any(isnum))
    x = as.data.frame(lapply(x[, isnum, drop = FALSE], normalize2, method = method, range = range))
  return(x)
}

normalize2 = function(x, method, range) {
  switch(method,
    range = (x - min(x)) / diff(range(x)) * diff(range) + range[1L],
    standardize = scale(x, center = TRUE, scale = TRUE),
    center = scale(x, center = TRUE, scale = FALSE),
    scale = scale(x, center = FALSE, scale = sd(x, na.rm = TRUE))
  )
}

