#' Replacement for names which always returns a vector.
#'
#' A simple wrapper for \code{\link[base]{names}}.
#' Returns a vector even if no names attribute is set (and not \code{NULL}).
#' Missing names are returned as \code{NA}, not \code{""}.
#'
#' @param x
#'   Object, probably named.
#' @return [\code{character}]: vector of the same length as \code{x}.
#' @export
#' @examples
#' x <- 1:3
#' names(x)
#' names2(x)
#' names(x[1:2]) = letters[1:2]
#' names(x)
#' names2(x)
names2 = function(x) {
  ns = names(x)
  if (is.null(ns))
    return(rep.int(NA_character_, length(x)))
  replace(ns, ns == "", NA_character_)
}
