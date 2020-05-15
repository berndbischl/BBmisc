#' @title Convert a list of row-vector of equal structure to a data.frame.
#'
#' @description
#' Elements are arranged in columns according to their name in each
#' element of \code{rows}.
#' Variables that are not present in some row-lists, or encoded as \code{NULL}, are filled using NAs.
#'
#' @param rows [\code{list}]\cr
#'   List of rows. Each row is a list or vector of the same structure,
#'   where all corresponding elements must have the same class.
#'   It is allowed that in some rows some elements are not present, see above.
#' @param strings.as.factors [\code{logical(1)}]\cr
#'   Convert character columns to factors?
#'   Default is \code{default.stringsAsFactors()} for R < "4.1.0" and \code{FALSE} otherwise.
#' @param row.names [\code{character} | \code{integer} | \code{NULL}]\cr
#'   Row names for result.
#'   By default the names of the list \code{rows} are taken.
#' @param col.names [\code{character} | \code{integer}]\cr
#'   Column names for result.
#'   By default the names of an element of \code{rows} are taken.
#' @return [\code{data.frame}].
#' @export
#' @examples
#' convertListOfRowsToDataFrame(list(list(x = 1, y = "a"), list(x = 2, y = "b")))
convertListOfRowsToDataFrame = function(rows, strings.as.factors = NULL,
  row.names, col.names) {
  assertList(rows)
  assertList(rows, types = "vector")
  if (!length(rows))
    return(makeDataFrame(0L, 0L))
  if (is.null(strings.as.factors)) {
    if(getRversion() < "4.1.0")
      strings.as.factors = default.stringsAsFactors()
    else
      strings.as.factors = FALSE
  }
  assertFlag(strings.as.factors)

  if (missing(row.names))
    row.names = names(rows)

  # make names
  rows = lapply(rows, function(x) setNames(x, make.names(names2(x, ""), unique = TRUE)))

  cols = unique(unlist(lapply(rows, names2)))
  if (anyMissing(cols))
    stop("All row elements must be named")
  if (!length(cols))
    return(makeDataFrame(length(rows), 0L))

  extract = function(cn) {
    tmp = lapply(rows, function(x) if (is.list(x)) x[[cn]] else unname(x[cn]))
    if (any(viapply(tmp, length) > 1L))
      stop("Rows may only contain a single value per name")
    simplify2array(replace(tmp, vlapply(tmp, is.null), NA))
  }

  d = data.frame(setNames(lapply(cols, extract), cols), row.names = row.names, stringsAsFactors = strings.as.factors)
  if (!missing(col.names))
    colnames(d) = col.names
  return(d)
}
