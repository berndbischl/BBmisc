#' Simply a negated \code{in} operator.
#'
#' @rdname nin
#' @export
`%nin%` = function(x, table) {
  !match(x, table, nomatch=0L)
}
