#' Do a function call with a timeout in seconds.
#' 
#' This is what internally happens: .... Bla
#'
#' @param f [\code{function}]\cr
#'   Function to aplly to \code{args}.
#' @param args [\code{list}]\cr
#'   Arguments for \code{f}.
#' @param timeout [\code{integer(1)}]\cr
#'   Tiemout in 
#'   Can not be used in combination with \code{chunks.size}
#' @param val [any]\cr
#'   Return this in case of timeout.
#'   Default is \code{NULL}.
#' @param off [\code{logical(1)}]\cr
#'   Switch off the whole procedure and simply do \code{f(args)}?
#'   For debugging and testing.
#'   Default is \code{FALSE}.
#' @return Either \code{f(args)} or \code{val}.
callWithTimeout = function(f, args, timeout, val = NULL, off = FALSE) {
  checkArg(f, "function")
  checkArg(list, "function")
  timeout = convertInteger(timeout)
  checkArg(timeout, "integer", len=1L, na.ok=FALSE)
  checkArg(off, "logical", len=1L, na.ok=FALSE)

  if (off) {
    return(do.call(f, args))
  }
  #FIXME where put this stuff? tmp? and handle parallel calls!
  #fn.image  = "cwto_image.RData"
  #fn.call = "cwto_call.RData"
  #fn.script = "cwto_script.R"
  #fn.returnval = "cwto_returnval.RData"
  fn.image  = tempfile()
  fn.call = tempfile()
  fn.script = tempfile()
  fn.returnval = tempfile()
  fn.rout = tempfile()
  
  save.image(file=fn.image)
  save2(file=fn.call, cwto.fun = f, cwto.args = args)
  catf(file=fn.script, "library(BBmisc)")
  catf(file=fn.script, append=TRUE, "load('%s')", fn.image)
  catf(file=fn.script, append=TRUE, "thecall = load2('%s')", fn.call)
  catf(file=fn.script, append=TRUE, "returnval = do.call(thecall$cwto.fun, thecall$cwto.args)")
  catf(file=fn.script, append=TRUE, "save2(file='%s', returnval=returnval)", fn.returnval)
  rcmd = sprintf("R CMD BATCH --no-save --no-restore %s %s", fn.script, fn.rout)
  sys3.args = c(as.character(timeout), rcmd)
  z = system3("timeout", sys3.args, wait=TRUE, stop.on.exit.code=FALSE)  
  if (z$exit.code == 0L) {
    # all ok, load result file
    return(load2(fn.returnval))
  } else if (z$exit.code == 124L) {
    # timeout, return default val
    return(val)
  } else {
    # should not happen
    # FIXME message
    stop("foooo")
  }
  # FIXME What to give the user in case of errors somewhere? on what levelvs can errors happen?
  # FIXME give logging?
  # FIXME what to do on clean up and exit?
}

ff = function(x) {
 Sys.sleep(5)
 return(x)
}

y = callWithTimeout(ff, list(x=1), timeout=2, val=99, off = TRUE)
print(y)
