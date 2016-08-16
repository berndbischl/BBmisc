#' Blow up single scalars / objects to vectors /  list by replication.
#'
#' Useful for standard argument conversion where a user can input a single
#' element, but this has to be replicated now n times for a resulting vector or list.
#'
#' @param x [any]\cr
#'   Input element.
#' @param n [\code{integer(1)}]\cr
#'   Desired length.
#'   Default is 1 (the most common case).
#' @param cl [\code{character}]\cr
#'   Only do the operation if \code{x} inherits from this one of these classes,
#'   otherwise simply let \code{x} pass.
#'   Default is \code{NULL} which means to always do the operation.
#' @param names [\code{character}*] \cr
#'   Names for result.
#'   Default is \code{NULL}, which means no names.
#' @param ensure.list [\code{logical(1)}]\cr
#'   Should \code{x} be wrapped in a list in any case?
#'   Default is \code{FALSE}, i.e., if \code{x} is a scalar value, a vector is
#'   returned.
#' @return Ether a vector or list of length \code{n} with replicated \code{x} or \code{x} unchanged..
#' @export
ensureVector = function(x, n = 1L, cl = NULL, names = NULL, ensure.list = FALSE) {
  n = convertInteger(n)
  assertCount(n)
  assertFlag(ensure.list)

  doit = isScalarValue(x) || !is.atomic(x)
  if (!is.null(cl)) {
    assertCharacter(cl, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
    doit = doit && inherits(x, cl)
  }

  if (doit) {
    if (isScalarValue(x) && !ensure.list) {
      xs = rep(x, n)
    } else {
      xs = replicate(n, x, simplify = FALSE)
    }

    if (!is.null(names)) {
      assertCharacter(names, len = n, any.missing = FALSE)
      names(xs) = names
    }
    return(xs)
  } else {
    return(x)
  }
}

