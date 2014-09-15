prettyPrint = function(x, indent.long = NULL, prefix = "", sep = ": ", key.align = "left", width = getOption("width") - 1L) {
  assertList(x, names = "named")
  assertString(prefix)
  assertString(sep)
  assertChoice(key.align, c("left", "right"))
  assertCount(width)

  keys = names(x)
  nc.key = max(nchar(keys))
  nc.prefix = nchar(prefix)
  nc.sep = nchar(sep)
  nc.left = nc.prefix + nc.key + nc.sep
  nc.right = max(width - nc.left, 0L)
  nc.indent = ifelse(is.null(indent.long), nc.left, asCount(indent.long))
  fmt = ifelse(key.align == "left", "%s%%-%is%s%%s", "%s%%%is%s%%s")
  fmt = sprintf(fmt, prefix, nc.key, sep)

  width.before = getOption("width", 80L)
  on.exit(options(width = width.before))
  options(width = min(max(width - nc.indent, 10L), 10000L))

  for (i in seq_along(x)) {
    val = x[[i]]
    if (is.atomic(val)) {
      if (!is.character(val))
        val = convertToShortString(val)
      lens = nchar(val)
      if (any(lens > nc.right)) {
        val = strwrap(val, width = width, indent = nc.left, exdent = nc.prefix + nc.indent)
        val[1L] = sub("^[[:space:]]+", "", val[1L])
      }
      val = collapse(val, "\n")
    } else {
      val = paste0(prefix, strrepeat(" ", nc.indent), capture.output(print(val)))
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
