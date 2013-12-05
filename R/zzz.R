#' @importFrom functional CurryL
.onLoad = function(libname, pkgname) {
  options(BBmisc.ProgressBar.style = getOption("BBmisc.ProgressBar.style", "text"))
  options(BBmisc.ProgressBar.width = getOption("BBmisc.ProgressBar.width", getOption("width")))
}
