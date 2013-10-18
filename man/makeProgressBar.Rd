\name{makeProgressBar}
\alias{ProgressBar}
\alias{makeProgressBar}
\title{Create a progress bar with estimated time.}
\usage{
  makeProgressBar(min = 0, max = 100, label = "",
    char = "+")
}
\arguments{
  \item{min}{[\code{numeric(1)}]\cr Minimum value, default
  is 0.}

  \item{max}{[\code{numeric(1)}]\cr Maximum value, default
  is 100.}

  \item{label}{[\code{character(1)}]\cr Label shown in
  front of the progress bar.  Note that if you later set
  \code{msg} in the progress bar function, the message will
  be left-padded to the length of this label, therefore it
  should be at least as long as the longest message you
  want to display.  Default is \dQuote{}.}

  \item{char}{[\code{character(1)}]\cr A single character
  used to display progress in the bar.  Default is
  \sQuote{+}.}
}
\value{
  [\code{\link{ProgressBar}}]. A list with following
  functions: \item{set [\code{function(value,
  msg=label)}]}{Set the bar to a value and possibly display
  a message instead of the label.} \item{inc
  [\code{function(value, msg=label)}]}{Increase the bar and
  possibly display a message instead of the label.}
  \item{kill [\code{function(clear=FALSE)}]}{Kill the bar
  so it cannot be used anymore. Cursor is moved to new
  line. You can also erase its display.} \item{error
  [\code{function(e)}]}{Useful in \code{tryCatch} to
  properly display error messages below the bar. See the
  example.}
}
\description{
  Create a progress bar function that displays the
  estimated time till completion and optional messages.
  Call the returned functions \code{set} or \code{inc}
  during a loop to change the display. Note that you are
  not allowed to decrease the value of the bar. If you call
  these function without setting any of the arguments the
  bar is simply redrawn with the current value. For
  errorhandling use \code{error} and have a look at the
  example below. You can globally change the behavior of
  all bars by setting the option
  \code{options(BBmisc.ProgressBar.style)} either to
  \dQuote{text} (the default) or \dQuote{off}, which
  display no bars at all. You can globally change the width
  of all bars by setting the option
  \code{options(BBmisc.ProgressBar.width)}. By default this
  is \code{getOption("width")}.
}
\examples{
bar <- makeProgressBar(max=5, label="test-bar")
for (i in 0:5) {
  bar$set(i)
  Sys.sleep(0.2)
}
bar <- makeProgressBar(max=5, label="test-bar")
for (i in 1:5) {
  bar$inc(1)
  Sys.sleep(0.2)
}
# display errors properly (in next line)
\dontrun{
f = function(i) if (i>2) stop("foo")
bar <- makeProgressBar(max=5, label="test-bar")
for (i in 1:5) {
  tryCatch ({
    f(i)
    bar$set(i)
  }, error=bar$error)
}
}
}

