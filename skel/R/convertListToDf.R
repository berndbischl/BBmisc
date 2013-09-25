#FIXME what is this actually useful for? it was from BJ
list2df = function(li, force.names=FALSE, strings.as.factors = default.stringsAsFactors()) {
  if (length(li) == 0L)
    return(as.data.frame(matrix(nrow = 0L, ncol = 0L)))

  if (force.names) {
    li = lapply(li, function(x) setNames(x, make.names(names2(x, ""), unique=TRUE)))
  }

  cols = unique(unlist(lapply(li, names)))

  if (length(cols) == 0L)
    return(as.data.frame(matrix(nrow = length(li), ncol = 0L)))

  res = namedList(cols)
  for(col in cols) {
    tmp = lapply(li, function(it) it[[col]])
    res[[col]] = simplify2array(replace(tmp, vapply(tmp, is.null, logical(1L)), NA))
  }
  as.data.frame(res, stringsAsFactors = strings.as.factors)
}
