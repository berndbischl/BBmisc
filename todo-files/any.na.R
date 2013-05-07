#' @useDynLib BBmisc
any.na = function(x)
  .Call("R_any_na", x)
