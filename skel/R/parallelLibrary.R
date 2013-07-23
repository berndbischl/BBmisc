#' Load packages on slaves.
#'
#' In case of snowfall mode, uses a combination of
#' \code{\link[snowfall]{sfClusterEval}} and \code{\link{require}}
#' to load the packages. For all other modes, this is not needed and the
#' function does nothing.
#'
#' @param packages [\code{character}]\cr
#'   Names of packages to load.
#' @param level [\code{character(1)}]\cr
#'   The function only loads the packages if the same level is specified in
#'   \code{\link{parallelStart}} or this argument is \code{NA}.
#'   See \code{\link{parallelMap}}. 
#'   Default is \code{NA}.
#' @return Nothing.
#' @export
parallelLibrary = function(packages, level=as.character(NA)) {
  checkArg(packages, "character", na.ok=FALSE)
  checkArg(level, "character", len=1L, na.ok=TRUE)
  
  if (getOption("BBmisc.parallel.mode") == "snowfall" && 
        (is.na(getOption("BBmisc.parallel.level")) || 
           getOption("BBmisc.parallel.level") == level)) {
    # sfLibrary chatters to much...
    for (p in packages) {
      sfClusterEval(require(p, character.only=TRUE))
    }
  }
}