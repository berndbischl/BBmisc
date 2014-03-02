#' Find the index of first/last \code{TRUE} value in a logical vector.
#'
#' @param x [\code{logical}]\cr
#'   Logical vector.
#' @param na.omit [\code{logical(1)}]\cr
#'   Should NAs be omitted from \code{x}?.
#'   Default is \code{TRUE}.
#' @return [\code{integer(1)} | \code{integer(0)}]. Returns the index of the first/last \code{TRUE}
#'   value in \code{x} or an empty integer vector if none is found.
#'   If NAs are encountered before a \code{TRUE} and not omitted, the result is \code{NA_integer_}.
#' @export
#' @useDynLib BBmisc c_first
findFirst = function(x, na.omit=TRUE) {
  .Call(c_first, x, na.omit, package="BBmisc")
}

#' @rdname findFirst
#' @export
#' @useDynLib BBmisc c_last
findLast = function(x, na.omit=TRUE) {
  .Call(c_last, x, na.omit, package="BBmisc")
}


