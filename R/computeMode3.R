#' computeMode(c(1,2,3,3))
computeMode3 = function(x, ties.method = "random", na.rm = TRUE) {

  tm = switch(ties.method, "first" = 1L, "last" = 2L, "random" = 3L)

  if (is.integer(x)) {
    shift = -min(x) + 1L
    if (shift > 0)
      x = x + shift
    mode = .Call("c_compute_mode", x, tm, PACKAGE = "BBmisc")
    if (shift > 0)
      mode = mode - shift
    return(mode)
  } else if (is.numeric(x) || is.character(x)) {
    f = as.factor(x)
    mode = .Call("c_compute_mode", as.integer(f), tm, PACKAGE = "BBmisc")
    mode = levels(f)[mode]
    if (is.numeric(x))
      mode = as.numeric(mode)
    return(mode)
  } else if (is.logical(x)) {
    x = as.integer(x) + 1L
    mode = .Call("c_compute_mode", x, tm, PACKAGE = "BBmisc")
    return(as.logical(mode - 1L))
  }

}

