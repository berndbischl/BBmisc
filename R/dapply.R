dapply = function(x, fun, ..., col.names, quick=TRUE) {
  # FIXME finish this
  checkArg(quick, "logical", len=1L, na.ok=FALSE)

  x = lapply(x, fun, ...)

  if (missing(col.names)) {
    ns = names2(x)
    missing = which(is.na(ns))
    if (length(missing))
      names(x) = replace(ns, missing, paste0("Var.", missing))
  } else {
    checkArg(col.names, "character", len=ncol(x), na.ok=FALSE)
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
  x
}
