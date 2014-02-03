vlapply = function(X, FUN, ..., USE.NAMES=TRUE) {
  vapply(X, FUN, NA, ..., USE.NAMES=USE.NAMES)
}

viapply = function(X, FUN, ..., USE.NAMES=TRUE) {
  vapply(X, FUN, NA_integer_, ..., USE.NAMES=USE.NAMES)
}

vdapply = function(X, FUN, ..., USE.NAMES=TRUE) {
  vapply(X, FUN, NA_real_, ..., USE.NAMES=USE.NAMES)
}

vcapply = function(X, FUN, ..., USE.NAMES=TRUE) {
  vapply(X, FUN, NA_complex_, ..., USE.NAMES=USE.NAMES)
}

vsapply = function(X, FUN, ..., USE.NAMES=TRUE) {
  vapply(X, FUN, NA_character_, ..., USE.NAMES=USE.NAMES)
}
