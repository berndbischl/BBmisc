#' Functions to determine the operating system.
#' 
#' \code{getOperatingSystem}: Simple wrapper for \code{.Platform$OS.type}, returns \code{character(1)}.
#' \code{isWindows}: Predicate for OS string, returns \code{logical(1)}.
#' \code{isUnix}: Predicate for OS string, returns \code{logical(1)}.
#'
#' @return See above.
#' @export
getOperatingSystem = function() {
  .Platform$OS.type
}

#' @rdname getOperatingSystem
#' @export
isWindows = function() {
  grepl("windows", getOperatingSystem(), ignore.case=TRUE)
}

#' @rdname getOperatingSystem
#' @export
isUnix = function() {
  grepl("unix", getOperatingSystem(), ignore.case=TRUE)
}

#FIXME mac? and wwhat else?