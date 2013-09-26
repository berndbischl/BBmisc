#' Convert a list of row-lists of equal structure to a data.frame.
#'
#' @param rows [\code{list}]\cr
#'   List of list of rows, where each inner list is of the same structure.
#' @param force.names [\code{logical(1)}]\cr
#'   Convert character columns to factors?
#'   Default is \code{default.stringsAsFactors()}.
#' @param strings.as.factors [\code{logical(1)}]\cr
#'   Convert character columns to factors?
#'   Default is \code{default.stringsAsFactors()}.
#' @param rows [\code{list}]\cr
#'   List of list of rows, where each inner list is of the same structure.
#' @param row.names [\code{character(nrow)} | \code{NULL}]\cr
#'   Row names.
#'   Default is \code{NULL}.
#' @param col.names [\code{character(ncol)}]\cr
#'   Column names.
#'   Default is \dQuote{V1}, \dQuote{V2}, and so on.
#' @return [\code{data.frame}].
#' @export
#' @examples
#' convertListofRowsToDataFrame(list(list(x=1, y="a"), list(x=2, y="b")))
#FIXME reread check and test
convertListofRowsToDataFrame = function(rows, force.names=FALSE, strings.as.factors = default.stringsAsFactors(),
                                        row.names=NULL, col.names) {
  
  n = length(rows)
  if (n == 0L)
    return(as.data.frame(matrix(nrow = 0L, ncol = 0L)))
  
  if (force.names) {
    rows = lapply(rows, function(r) setNames(r, make.names(names2(r, ""), unique=TRUE)))
  }
  
  cols = unique(unlist(lapply(rows, names)))
  
  if (length(cols) == 0L)
    return(as.data.frame(matrix(nrow = n, ncol = 0L)))
  
  res = namedList(cols)
  for(col in cols) {
    tmp = lapply(rows, function(r) r[[col]])
    res[[col]] = simplify2array(replace(tmp, vapply(tmp, is.null, logical(1L)), NA))
  }
  as.data.frame(res, stringsAsFactors = strings.as.factors)
}
