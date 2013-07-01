#' Converts list to short string describing contents.
#'
#' Looks like this \dQuote{a=1,<unamed>=2,b=<data.frame>}.
#' Vectors are displayed till a certain length.
#'
#' @param xs [\code{list}]\cr
#'   The list.
#' @return [\code{character(1)}].
#' @export
#' @examples
#' listToShortString(list(a=1, b=NULL, "foo", c=1:10))
listToShortString = function(xs) {
  if (length(xs) == 0L)
    return("")
  ns = names2(xs, missing.val = "<unnamed>")
  ss = lapply(xs, function(x) {
    if (is.atomic(x)) {
      if (length(x) <= 1L)
        x
      else
        capture.output(str(x, nchar.max=10L, give.attr=FALSE, give.head=FALSE, indent.str=""))
    } else {
      paste("<", class(x)[1L], ">", sep="")
    }
  })
  paste(paste(ns, "=", ss, sep=""), collapse=", ")
}
