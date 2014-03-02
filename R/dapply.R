#' Call \code{lapply} on an object and return a data.frame.
#
#' @param x [\code{data.frame}]\cr
#'   Data frame.
#' @param fun [\code{function}]\cr
#'   The function to apply.
#' @param ... [any]\cr
#'   Further arguments passed down to \code{fun}.
#' @param col.names [\code{character(1)}]\cr
#'   Column names for result.
#'   Default are the names of \code{x}.
#' @param quick [\code{logical(1)}]\cr
#'   ??????
#'   Default is \code{TRUE}.
#' @export
#' @return [\code{data.frame}].
dapply = function(x, fun, ..., col.names, quick=TRUE) {
  # FIXME: finish this
  # maybe better not check x type to be really general?
  checkArg(fun, "function")
  checkArg(quick, "logical", len=1L, na.ok=FALSE)

  x = lapply(x, fun, ...)

  if (missing(col.names)) {
    ns = names2(x)
    missing = which(is.na(ns))
    if (length(missing))
      names(x) = replace(ns, missing, paste0("Var.", missing))
  } else {
    checkArg(col.names, "character", len=length(x), na.ok=FALSE)
    names(x) = col.names
  }

  if (quick) {
    n = unique(viapply(x, length))
    if (length(n) > 1L) {
      max.n = max(n)
      if (any(max.n %% n))
        stop("Arguments imply differing number of rows: ", collapse(n, ", "))
      x = lapply(x, rep_len, length.out=max.n)
      n = max.n
    }
    attr(x, "row.names") = seq_len(n)
    attr(x, "class") = "data.frame"
  } else {
    x = data.frame(x, stringsAsFactors=FALSE)
  }
  return(x)
}
