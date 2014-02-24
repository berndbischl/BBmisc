#' Convert a list of row-vectors of equal structure to a data.frame.
#'
#' Elements are arranged in columns according to their name in each
#' element of \code{rows}. Missing values are filled using NAs.
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
#' @return [\code{data.frame}].
#' @export
#' @examples
#' convertListOfRowsToDataFrame(list(list(x=1, y="a"), list(x=2, y="b")))
convertListOfRowsToDataFrame = function(rows, strings.as.factors = default.stringsAsFactors(),
  row.names) {

  checkArg(rows, "list")
  checkListElementClass(rows, "vector")
  if (!length(rows))
    return(makeDataFrame(0L, 0L))
  checkArg(strings.as.factors, "logical", len=1L, na.ok=FALSE)

  if (missing(row.names)) {
    row.names = names(rows)
  } else {
    checkArg(row.names, "character", len=length(rows), na.ok=FALSE)
  }

  # make names
  rows = lapply(rows, function(x) setNames(x, make.names(names2(x, ""), unique=TRUE)))

  cols = unique(unlist(lapply(rows, names2)))
  if (any(is.na(cols)))
    stop("All row elements must be named")
  if (!length(cols))
    return(makeDataFrame(length(rows), 0L))

  extract = function(cn) {
    tmp = lapply(rows, function(x) if (is.list(x)) x[[cn]] else unname(x[cn]))
    if (any(viapply(tmp, length) > 1L))
      stop("Rows may only contain a single value per name")
    simplify2array(replace(tmp, vlapply(tmp, is.null), NA))
  }

  data.frame(setNames(lapply(cols, extract), cols), row.names=row.names, stringsAsFactors=strings.as.factors)
}
