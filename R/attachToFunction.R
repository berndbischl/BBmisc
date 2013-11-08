# FIXME document, export, test
attachToFunction = function(f, ...) {
  f = match.fun(f)
  ddd = argsAsNamedList(...)
  ok = isValidName(names(ddd))
  if (!all(ok))
    stopf("Invalid names found: %s", collapse(names(ddd)[!ok]))
  if (environmentName(environment(f)) == "R_GlobalEnv")
    f = clearFunctionEnv(f, inplace=FALSE)
  mapply(assign, x=names(ddd), value=ddd, MoreArgs=list(envir=environment(f)),
         USE.NAMES=FALSE, SIMPLIFY=FALSE)
  f
}
