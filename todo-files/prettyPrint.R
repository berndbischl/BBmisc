# Print objects in a pretty format
#
# @param x [\code{named list}]\cr
#  Named list of objects to print.
#  Will be printed using a \dQuote{[prefix][name][sep][value]} format.
#  Atomic vectors which are not already character will be coerced similar to
#  \code{\link{convertToShortString}}.
#  Character vectors will be treated as text and re-formated using \code{\link[base]{strwrap}}.
#  All non-atomic objects will be printed using their respective print method.
# @param indent.long [\code{integer(1)}]\cr
#  Number of spaces to indent multi-line output with.
#  Default is \code{NULL} which will result in a column-like alignment on \code{sep}.
# @param prefix [\code{character(1)}\cr
#  String used to prefix each item item of \code{x} with.
# @param sep [\code{character(1)}]\cr
#  String to separate name and value.
# @param name.align [\code{character(1)}]\cr
#  \dQuote{left} for left alignment, \dQuote{right} for right alignment of item names.
# @param width [\code{integer(1)}]\cr
#  Number of chars for output.
# @return Invisibly \code{TRUE}.
prettyPrint = function(x, indent.long = NULL, prefix = "", sep = ": ", name.align = "left", width = getOption("width", 80L)) {
  convertAtomicToString = function(x, width = getOption("width", 80L), sep = ",") {
    # similar to convertToShortString, but respects widths
    # -> unify!
    if (is.null(x))
      return("NULL")
    if (length(x) == 0L)
      return(sprintf("%s(0)", class(x)[1L]))
    if (is.double(x))
      x = sprintf("%.4f", x)
    x = as.character(x)
    if (length(x) == 1L)
      return(x)
    i = which.last(cumsum(nchar(x) + nchar(sep)) < width - 4L)
    paste0(collapse(head(x, i), sep), ",...")
  }

  assertList(x, names = "named")
  assertString(prefix)
  assertString(sep)
  assertChoice(name.align, c("left", "right"))
  assertCount(width)

  keys = names(x)
  nc.key = max(nchar(keys))
  nc.prefix = nchar(prefix)
  nc.sep = nchar(sep)
  nc.left = nc.prefix + nc.key + nc.sep
  nc.right = max(width - nc.left, 0L)
  nc.indent = ifelse(is.null(indent.long), nc.left, asCount(indent.long))
  fmt = ifelse(name.align == "left", "%s%%-%is%s%%s", "%s%%%is%s%%s")
  fmt = sprintf(fmt, prefix, nc.key, sep)

  width.before = getOption("width", 80L)
  on.exit(options(width = width.before))
  options(width = min(max(width - nc.indent, 10L), 10000L))

  for (i in seq_along(x)) {
    val = x[[i]]
    if (is.atomic(val)) {
      if (!is.character(val))
        val = prettyConvertAtomic(val, width = nc.right)
      lens = nchar(val)
      if (any(lens > nc.right)) {
        val = strwrap(val, width = width, indent = nc.left, exdent = nc.prefix + nc.indent)
        val[1L] = sub("^[[:space:]]+", "", val[1L])
      }
      val = collapse(val, "\n")
    } else {
      val = paste0(prefix, strrepeat(" ", nc.indent), printToChar(val, collapse = NULL))
      if (nc.indent == nc.left) {
        val[1L] = sub("^[[:space:]]+", "", val[1L])
        val = collapse(val, "\n")
      } else {
        val = paste0("\n", collapse(val, "\n"))
      }
    }
    cat(sprintf(fmt, keys[i], val), "\n")
  }
  invisible(TRUE)
}
