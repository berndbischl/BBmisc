#' @title Current time in seconds
#'
#' @description
#' Simple wrapper for \code{as.integer(Sys.time())}.
#'
#' @return [\code{integer(1)}].
#' @export
getUnixTime = function() {
  as.integer(Sys.time())
}
