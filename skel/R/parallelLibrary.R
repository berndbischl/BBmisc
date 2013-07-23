#' Load packages on slaves.
#'
#' In case of snowfall mode, uses a combination of
#' \code{\link[snowfall]{sfClusterEval}} and \code{\link{requitre}}
#' to load the packages. For all other modes, this is not needed and the
#' function does nothing.
#'
#' @param packages [\code{character}]\cr
#'   Names of packages to load.
#' @return Nothing.
#' @export
parallelLibrary = function(packages, level) {
  if (getOption("BBmisc.parallel.mode") == "snowfall" && 
        (is.na(getOption("BBmisc.parallel.level")) || 
           getOption("BBmisc.parallel.level") == level)) {
    # sfLibrary chatters to much...
    for (p in packages) {
      sfClusterEval(require(p, character.only=TRUE))
    }
  }
}