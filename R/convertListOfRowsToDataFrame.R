#' Convert a list of row-vectors of equal structure to a data.frame.
#'
#' @param rows [\code{list}]\cr
#'   List of rows. Each row is a list or vector of the same structure.
#'   That means all rows must have the same length and all corresponding elements must have the 
#'   same class.
#' @param strings.as.factors [\code{logical(1)}]\cr
#'   Convert character columns to factors?
#'   Default is \code{default.stringsAsFactors()}.
#' @param row.names [\code{character(nrow)} | \code{NULL}]\cr
#'   Row names for result.
#'   \code{NULL} implies no row names are set.
#'   By default the names of the list \code{rows} are taken.
#' @param col.names [\code{character(ncol)}]\cr
#'   Column names.
#'   Default the are the names of the elements of the first row, or, 
#'   if this row has no element names, \dQuote{V1}, \dQuote{V2}, 
#'   and so on, is used.
#' @return [\code{data.frame}].
#' @export
#' @examples
#' convertListOfRowsToDataFrame(list(list(x=1, y="a"), list(x=2, y="b")))
convertListOfRowsToDataFrame = function(rows, strings.as.factors = default.stringsAsFactors(), 
  row.names, col.names) {

  checkArg(rows, "list")
  checkListElementClass(rows, "vector")
  nrows = length(rows)
  if (nrows == 0L)
    return(as.data.frame(matrix(nrow=0L, ncol=0L)))
  checkArg(strings.as.factors, "logical", len=1L, na.ok=FALSE)
  if (missing(row.names)) {
    row.names = names(rows)
  } else {
    checkArg(row.names, "character", len=nrows, na.ok=FALSE)
  }
  ncols = length(rows[[1L]])
  if (ncols == 0L)
    return(as.data.frame(matrix(nrow=nrows, ncol=0L)))
  if (missing(col.names)) {
    if (isProperlyNamed(rows[[1]]))
      col.names = names(rows[[1]])
    else
      col.names = paste0("V", 1:ncols)
  } else {
    checkArg(col.names, "character", len=ncols, na.ok=FALSE)
  }

  # FIXME: check that rows have the same "structure" and length
  
  # force all names
  rows = lapply(rows, function(r) setNames(r, col.names))

  res = namedList(col.names)
  for(cn in col.names) {
    # pick out column element from each row
    col = lapply(rows, function(r) r[[cn]])
    # recode NULLs with NAs
    res[[cn]] = simplify2array(replace(col, vapply(col, is.null, logical(1L)), NA))
  }
  res = as.data.frame(res, stringsAsFactors = strings.as.factors)
  rownames(res) = row.names
  return(res)
}
