# FIXME document, export, test
isValidName = function(x, unique=FALSE) {
  if (!is.character(x))
    x = as.character(x)
  x == make.names(x, isTRUE(unique)) & !grepl("^\\.\\.[0-9\\.]$", x)
}
