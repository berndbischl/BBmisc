#' Current time in seconds.
#' 
#' Simple wrapper for \code{as.integer(Sys.time())}.
#'
#' @return [\code{integer(1)}].
#' @export
# FIXME this is called unix time ...
getCurrentTimeSecs = function() {
  as.integer(Sys.time())
}

