#' Converts an arbitrary R object to a short, descriptive string so it can be used in messages.
#'
#' Atomics: If of length 0 or 1, they are basically printed as they are. Numerics are 
#' formated with \code{num.format}. If of length greater than 1, their \code{str} is printed
#' in a way so it does not become excessively long.
#'
#' All others: Currently, only their class is simply printed like \dQuote{<data.frame>}.
#'
#' Lists: The mechanism above is applied (non-recursively) to their elements. The result 
#' looks like this: \dQuote{a=1, <unamed>=2, b=<data.frame>, c=<list>}.
#'
#' @param x [any]\cr
#'   The object.
#' @param num.format [\code{character(1)}]\cr
#'   Used to format numerical scalars via \code{\link{sprintf}}.
#'   Default is \dQuote{\%.4g}.
#' @return [\code{character(1)}].
#' @export
#' @examples
#' listToShortString(list(a=1, b=NULL, "foo", c=1:10))
convertToShortString = function(x, num.format="%.4g") {

  # convert non-list object to string
  convObj = function(x) {
    if (is.atomic(x)) {
      if (length(x) == 0L) {
        sprintf("%s(0)", class(x)[1])
      } else if (length(x) == 1L) {
        if (is.double(x))
          sprintf(num.format, x)
        else
          paste(x)
      } else {
        capture.output(str(x, nchar.max=10L, give.attr=FALSE, give.head=FALSE, indent.str=""))
      }
    } else {
      paste("<", class(x)[1L], ">", sep="")
    }
  }

  # handle only lists and not any derived data types
  if (class(x)[1] == "list") {
    if (length(x) == 0L)
      return("")
    ns = names2(x, missing.val = "<unnamed>")
    ss = lapply(x, convObj)
    collapse(paste(ns, "=", ss, sep=""), ", ")
  } else {
    convObj(x)
  }
}


