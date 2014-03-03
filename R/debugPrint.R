debugPrint = function(..., header=FALSE) {
  small = function(name, obj) {
    message("*** '", name, "':")
    print(obj)
  }
  big = function(name, obj) {
    message(rep('*', options("width")))
    message("*** '", name)
    message(rep('*', options("width")))
    print(obj)
  }

  ddd = argsAsNamedList(...)
  Map(if (header) big else small, name=names(ddd), obj=ddd)
  invisible(NULL)
}
