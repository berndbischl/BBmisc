#' Create a progress bar with estimated time.
#'
#' Create a progress bar function that displays the estimated time till
#' completion and optional messages. Call the returned functions \code{set} or
#' \code{inc} during a loop to change the display.
#' Note that you are not allowed to decrease the value of the bar.
#' If you call these function without setting any of the arguments
#' the bar is simply redrawn with the current value.
#' For errorhandling use \code{error} and have a look at the example below.
#' You can globally change the behavior of all bars by setting the option
#' \code{options(BBmisc.ProgressBar.style)} either to \dQuote{text} (the default)
#' or \dQuote{off}, which display no bars at all.
#' You can globally change the width of all bars by setting the option
#' \code{options(BBmisc.ProgressBar.width)}. By default this is \code{getOption("width")}.
#'
#' @param min [\code{numeric(1)}]\cr
#'   Minimum value, default is 0.
#' @param max [\code{numeric(1)}]\cr
#'   Maximum value, default is 100.
#' @param label [\code{character(1)}]\cr
#'   Label shown in front of the progress bar.
#'   Note that if you later set \code{msg} in the progress bar function,
#'   the message will be left-padded to the length of this label, therefore
#'   it should be at least as long as the longest message you want to display.
#'   Default is \dQuote{}.
#' @param char [\code{character(1)}]\cr
#'   A single character used to display progress in the bar.
#'   Default is \sQuote{+}.
#' @return [\code{\link{ProgressBar}}]. A list with following functions:
#'   \item{set [\code{function(value, msg=label)}]}{Set the bar to a value and possibly display a message instead of the label.}
#'   \item{inc [\code{function(value, msg=label)}]}{Increase the bar and possibly display a message instead of the label.}
#'   \item{kill [\code{function(clear=FALSE)}]}{Kill the bar so it cannot be used anymore. Cursor is moved to new line. You can also erase its display.}
#'   \item{error [\code{function(e)}]}{Useful in \code{tryCatch} to properly display error messages below the bar. See the example.}
#' @export
#' @aliases ProgressBar
#' @examples
#' bar <- makeProgressBar(max=5, label="test-bar")
#' for (i in 0:5) {
#'   bar$set(i)
#'   Sys.sleep(0.2)
#' }
#' bar <- makeProgressBar(max=5, label="test-bar")
#' for (i in 1:5) {
#'   bar$inc(1)
#'   Sys.sleep(0.2)
#' }
#' # display errors properly (in next line)
#' \dontrun{
#' f = function(i) if (i>2) stop("foo")
#' bar <- makeProgressBar(max=5, label="test-bar")
#' for (i in 1:5) {
#'   tryCatch ({
#'     f(i)
#'     bar$set(i)
#'   }, error=bar$error)
#' }
#' }
makeProgressBar = function(min = 0, max = 100, label = "", char = "+", log = NULL) {
  checkArg(min, "numeric", len=1L, na.ok=FALSE)
  checkArg(max, "numeric", len=1L, na.ok=FALSE)
  if (!(min < max))
    stop("Argument max must be greater than min")
  checkArg(label, "character", len=1L)
  if (!is.null(log)) {
    checkArg(log, "character", len=1L, na.ok=FALSE)
    if (file.exists(log))
      file.remove(log)
  }

  style = getOption("BBmisc.ProgressBar.style", "text")
  if (!is.character(style) || length(style) > 1L || style %nin% c("text", "off"))
    stop("BBmisc.ProgressBar.style option must be one of: 'text', 'off'!")
  if (style == "off")
     return(structure(list(
       set = function(value, msg) invisible(NULL),
       inc = function(inc, msg) invisible(NULL),
       kill =  function(clear=FALSE) invisible(NULL),
       error = function(e) stop(e)
     ), class = "ProgressBar"))

  strrepeat = function(x, n, sep = "") collapse(rep.int(x, max(0L, n)), sep)
  now = function() as.integer(Sys.time())
  convertLabel = function(x) if(!is.na(x) && nchar(x)) sprintf("%s ", x) else ""
  getPerc = function() (self$state - self$from) / abs(self$from - self$to)
  getWidth = function() getOption("BBmisc.ProgressBar.width", getOption("width"))

  self = list(from = min, to = max, state = min, start.time = now(),
              label = convertLabel(label), char = char, last.line = "",
              killed = FALSE, log = log)
  self = as.environment(self)
  rm(list = c("min", "max", "label", "char", "log"))


  output = function() {
    perc = getPerc()
    eta = splitTime((1 - perc) * (now() - self$start.time) / (perc+0.001), unit = "hours")
    if (eta["hours"] > 99L)
      eta[] = 99L
    label = self$label
    nc.label = nchar(label)
    width = getWidth()
    format.required.chars = 20L
    pb.required.chars = 10L
    pb.width = width - format.required.chars - nc.label

    # fix for small text width: try to trim the label or drop progressbar
    if (pb.width < 10L) {
      trim = width - format.required.chars - pb.required.chars
      if (trim <= 5L) {
        pb.width = pb.width - nc.label
        label = ""
        nc.label = 0L
      } else {
        pb.width = pb.width - nc.label + trim + 4L
        label = sprintf("%s... ", strtrim(label, trim))
        nc.label = trim + 4L
      }
      pb.width = width - format.required.chars - nc.label
    }

    if (pb.width > pb.required.chars) {
      chars.progress = floor(perc * pb.width)
      s = sprintf("%s|%s%s| %5.1f%% (%02i:%02i:%02i)",
                  label,
                  strrepeat(self$char, chars.progress),
                  strrepeat(" ", pb.width - chars.progress),
                  perc * 100,
                  eta["hours"], eta["minutes"], eta["seconds"])
    } else {
      s = sprintf("%5.1f%% (%02i:%02i:%02i)",
                  perc * 100,
                  eta["hours"], eta["minutes"], eta["seconds"])
    }
    catf("\r%s%s", s, strrepeat(" ", nchar(self$last.line) - nchar(s)), newline=FALSE)
    flush.console()
    self$last.line = s
  }

  kill = function(clear = FALSE) {
    self$killed = TRUE
    if (clear) {
      catf("\r%s", strrepeat(" ", nchar(self$last.line)))
    } else {
      output()
      cat("\n")
    }
    flush.console()
  }

  set = function(value, msg) {
    if (self$killed)
      stop("Progressbar is already killed")

    if (!missing(value)) {
      checkArg(value, "numeric", lower = self$from, upper = self$to, na.ok = FALSE)
      if (value >= self$to) {
        self$state = self$to
        kill()
      } else {
        self$state = value
        output()
      }
    }

    if (!missing(msg)) {
      checkArg(msg, "character", len = 1L)
      self$label = convertLabel(msg)
    }
  }

  inc = function(inc = 1L, label) {
    set(value + inc, label)
  }

  error = function(e) {
    kill()
    stop(e)
  }

  log = function(msg) {
    if (is.null(self$log))
      stop("Log file not specified")
    catf("[%5.1f%%] <%s> %s", getPerc()*100, Sys.time(), msg, file = self$log, append = TRUE)
  }

  structure(list(set = set, kill = kill, inc = inc, error = error, log = log), class = "progressbar")
}
# makeProgressBar = function(min=0, max=100, label="", char="+") {
#   checkArg(min, "numeric", len=1L, na.ok=FALSE)
#   checkArg(max, "numeric", len=1L, na.ok=FALSE)
#   checkArg(label, "character", len=1L, na.ok=FALSE)
#
#   style = getOption("BBmisc.ProgressBar.style", "text")
#   if (!is.character(style) || length(style) > 1L || !(style %in% c("text", "off")))
#     stop("BBmisc.ProgressBar.style option must be one of: 'text', 'off'!")
#   if (style == "off")
#     return(structure(list(
#       set = function(value, msg=label) invisible(NULL),
#       inc = function(inc, msg=label) invisible(NULL),
#       kill =  function(clear=FALSE) invisible(NULL),
#       error = function(e) stop(e)
#     ), class = "ProgressBar"))
#   width = getOption("BBmisc.ProgressBar.width", getOption("width"))
#   width = convertInteger(width)
#   if (!is.integer(width) || length(width) != 1 || is.na(width) || width <= 0L || width < 30L)
#     stop("BBmisc.ProgressBar.width option must be a positive integer >= 30!")
#   if (style == "off")
#     return(function(...) invisible(NULL))
#   ## label |................................| xxx% (hh:mm:ss)
#   label.width = nchar(label)
#   bar.width = width - label.width - 21L
#   bar = rep(" ", bar.width)
#
#   start.time = as.integer(Sys.time())
#   delta = max - min
#   kill.line = "\r"
#   killed = FALSE
#   cur.value = min
#   draw = function(value, inc, msg) {
#     if (!missing(value) && !missing(inc))
#       stop("You must not set value and inc!")
#     else if (!missing(value))
#       checkArg(value, "numeric", len=1L, na.ok=FALSE, lower=max(min,cur.value), upper=max)
#     else if (!missing(inc)) {
#       checkArg(inc, "numeric", len=1L, na.ok=FALSE, lower=0, upper=max-cur.value)
#       value = cur.value + inc
#     } else {
#       value = cur.value
#     }
#     if (!killed)  {
#       # special case for min == max, weird "empty" bar, but might happen...
#       if (value == max)
#         rate = 1
#       else
#         rate = (value - min) / delta
#       bin = round(rate * bar.width)
#       bar[seq(bin)] <<- char
#       delta.time = as.integer(Sys.time()) - start.time
#       if (value == min)
#         rest.time = 0
#       else
#         rest.time = (max - value) * (delta.time / (value - min))
#       rest.time = splitTime(rest.time, "hours")
#       # as a precaution, so we can _always_ print in the progress barcat
#       if (rest.time["hours"] > 99)
#         rest.time[] = 99
#       cat(kill.line)
#       msg = sprintf(sprintf("%%%is", label.width), msg)
#       catf("%s |%s| %3i%% (%02i:%02i:%02i)", newline=FALSE,
#            msg, collapse(bar, sep=""), round(rate*100),
#            rest.time["hours"], rest.time["minutes"], rest.time["seconds"])
#       if (value == max)
#         kill()
#       flush.console()
#     }
#     cur.value <<- value
#   }
#   clear = function(newline=TRUE) {
#     cat(kill.line)
#     cat(rep(" ", width))
#     if (newline)
#       cat("\n")
#   }
#   kill = function(clear=FALSE) {
#     if (clear)
#       clear(newline=TRUE)
#     else
#       cat("\n")
#     killed <<- TRUE
#   }
#   structure(list(
#     set = function(value, msg=label) draw(value=value, msg=msg),
#     inc = function(inc, msg=label) draw(inc=inc, msg=msg),
#     kill = kill,
#     error = function(e) {
#       kill(clear=FALSE)
#       stop(e)
#     }
#   ), class = "ProgressBar")
# }
