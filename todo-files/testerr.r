library(BBmisc)

tryCatchDebug = function(expr) {
  error.handler = function(e) {
    dump.frames(dumpto = ".dump.tmp")
    ee$dump = get(".dump.tmp", envir = .GlobalEnv)
    rm(list = ".dump.tmp", envir = .GlobalEnv)
    capture.output({ ee$traceback = traceback(1) })
    ee$calls = sys.calls()
    e
  }

  ee = new.env()
  res = list(value = try(withCallingHandlers(expr, error = error.handler)))

  if (is.error(res$value)) {
    res$traceback = ee$traceback
    res$dump = ee$dump
    res$calls = ee$calls
    res$msg = as.character(res$value)
  }
  res
}

g <- function(x) stop(x)
f <- function() { x = "michel"; y = "warning"; g(x) }
rm(x); x = tryCatchDebug(f())

