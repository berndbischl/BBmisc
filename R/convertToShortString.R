#' @title Converts any R object to a descriptive string so it can be used in messages.
#'
#' @description
#' Atomics: If of length 0 or 1, they are basically printed as they are.
#' Numerics are formated with \code{num.format}.
#' If of length greater than 1, they are collapsed witd \dQuote{,} and clipped.
#' so they do not become excessively long.
#' Expressions will be converted to plain text.
#'
#' All others: Currently, only their class is simply printed
#' like \dQuote{<data.frame>}.
#'
#' Lists: The mechanism above is applied (non-recursively) to their elements.
#' The result looks like this:
#' \dQuote{a=1, <unamed>=2, b=<data.frame>, c=<list>}.
#'
#' @param x [any]\cr
#'   The object.
#' @param num.format [\code{character(1)}]\cr
#'   Used to format numerical scalars via \code{\link{sprintf}}.
#'   Default is \dQuote{\%.4g}.
#' @param clip.len [\code{integer(1)}]\cr
#'   Used clip atomic vectors via \code{\link{clipString}}.
#'   Default is 15.
#' @return [\code{character(1)}].
#' @export
#' @examples
#' convertToShortString(list(a = 1, b = NULL, "foo", c = 1:10))
convertToShortString = function(x, num.format = "%.4g", clip.len = 15L) {

  # convert non-list object to string
  convObj = function(x) {
    cl = getClass1(x)
    string = 
      if (is.atomic(x) && !is.null(x) && length(x) == 0L)
        sprintf("%s(0)", getClass1(x))
      else if (cl == "numeric")
        paste(sprintf(num.format, x), collapse=",")
      else if (cl == "integer")
        paste(as.character(x), collapse=",")
      else if (cl == "logical")
        paste(as.character(x), collapse=",")
      else if (cl == "character")
        collapse(x)
      else if (cl == "expression")
        as.character(x)
      else
        sprintf("<%s>", cl)
    clipString(string, clip.len)
  }

  # handle only lists and not any derived data types
  if (getClass1(x) == "list") {
    if (length(x) == 0L)
      return("list()")
    ns = names2(x, missing.val = "<unnamed>")
    ss = lapply(x, convObj)
    collapse(paste(ns, "=", ss, sep = ""), ", ")
  } else {
    convObj(x)
  }
}
