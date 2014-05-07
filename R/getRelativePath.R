# Construct a path relative to another
#
# Constructs a relative path from path \code{from} to path \code{to}.
# If this is not possible (i.e. different drive letters on windows systems),
# this function returns \code{NA}.
#
# @param to [\code{character(1)}]\cr
#  Where the relative path should point to.
# @param from [\code{character(1)}]\cr
#  From which part to start.
#  Default is \code{\link[base]{getwd}}.
# @return [character(1)]: A relative path.
# @export
getRelativePath = function(to, from = getwd()) {
  splitPath = function(path) {
    path = normalizePath(path, mustWork = FALSE)
    if (isWindows()) {
      pattern = "^([[:alpha:]]:)|(\\\\[[:alnum:]]+)"
      m = regexpr(pattern, path)
      if (length(m) == 1L && m == -1L)
        stop("Error extracting the drive letter")
      drive = regmatches(path, m)
      regmatches(path, m) = ""
    } else {
      drive = ""
    }
    list(drive = drive, path = Filter(nzchar, strsplit(path, "[/\\]+")[[1L]]))
  }

  numberCommonParts = function(p1, p2) {
    for (i in seq_len(min(length(p1), length(p2)))) {
      if (p1[i] != p2[i])
        return(i - 1L)
    }
    return(if (is.null(i)) 0L else i)
  }

  from = splitPath(from)
  to = splitPath(to)
  if (from$drive != to$drive)
    return(NA_character_)

  if (isWindows())
    i = numberCommonParts(tolower(from$path), tolower(to$path))
  else
    i = numberCommonParts(from$path, to$path)

  res = c(rep.int("..", length(from$path) - i), tail(to$path, -i))
  if (length(res) == 0L)
    res = "."
  collapse(res, .Platform$file.sep)
}
