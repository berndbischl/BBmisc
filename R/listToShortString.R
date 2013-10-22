#' Converts list to short string describing contents.
#'
#' Looks like this \dQuote{a=1,<unamed>=2,b=<data.frame>}.
#' Vectors are displayed up to a certain length.
#'
#' The function is deprecated, use \code{\link{convertToShortString}}.
#'
#' @param xs [\code{list}]\cr
#'   The list.
#' @param num.format [\code{list}]\cr
#'   Used to format numerical scalars via \code{\link{sprintf}}.
#'   Default is \dQuote{\%.4g}.
#' @return [\code{character(1)}].
# @export
#' @examples
#' listToShortString(list(a=1, b=NULL, "foo", c=1:10))
#FIXME remove, there is convertToShortString. copy tests then.
listToShortString = function(xs, num.format="%.4g") {
  if (length(xs) == 0L)
    return("")
  ns = names2(xs, missing.val = "<unnamed>")
  ss = lapply(xs, function(x) {
    if (is.atomic(x)) {
      if (length(x) <= 1L) {
        if (is.double(x))
          sprintf(num.format, x)
        else
          x
      } else {
        capture.output(str(x, nchar.max=10L, give.attr=FALSE, give.head=FALSE, indent.str=""))
      }
    } else {
      paste("<", class(x)[1L], ">", sep="")
    }
  })
  paste(paste(ns, "=", ss, sep=""), collapse=", ")
}


