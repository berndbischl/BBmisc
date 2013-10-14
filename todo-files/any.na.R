#' @useDynLib BBmisc
any.missing = function(x, inf.as.missing=TRUE)
  .Call("R_any_na", x)
