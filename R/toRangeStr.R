#' Convert a numerical vector into a range string.
#'
#' @param x [\code{integer}]\cr
#'   Vector to convert into a range string.
#' @param range.sep [\code{character(1)}]\cr
#'   Separator between the first and last element of a range of consecutive
#'   elements in \code{x}.
#'   Default is \dQuote{ - }.
#' @param block.sep [\code{character(1)}]\cr
#'   Separator between non consecutive elements of \code{x} or ranges.
#'   Default is \dQuote{, }.
#' @return [\code{character(1)}]
#' @examples
#' x <- sample(1:10, 7)
#' toRangeStr(x)
#' @export
toRangeStr = function(x, range.sep=" - ", block.sep=", ") {
  x = convertIntegers(x)
  checkArg(x, "numeric", na.ok=FALSE)
  checkArg(range.sep, "character", len=1, na.ok=FALSE)
  checkArg(block.sep, "character", len=1, na.ok=FALSE)
  
  findRange = function(x) {
    end_index = max(which(x == x[1L] + 0:(length(x)-1L)))
    1:end_index
  }
  sorted.x = sort(unique(x))
  res = c()
  while(length(sorted.x) > 0L) {
    # Find the longest head of sorted_x that has consecutive elements. 
    rng = findRange(sorted.x)
    # Extract the leading range
    head = sorted.x[rng]
    # Trim leading range.
    sorted.x = sorted.x[-rng]    
    res = if (length(head) == 1L) {
      c(res, format(head))
    } else {
      # Collapse range into "min(head) - max(head)" string.
      c(res, collapse(c(head[1L], head[length(head)]), range.sep))
    }
  }
  collapse(res, block.sep)
}


# FIXME no need for loops:
# x = sort(unique(x))
# x = unname(split(x, c(0L, cumsum(diff(x) > 1L))))
# combine = function(x) if (length(x) == 1L) as.character(x) else sprintf("%i - %i", x[1L], x[length(x)])
# collapse(vapply(x, combine, character(1L), USE.NAMES=FALSE), ", ")

