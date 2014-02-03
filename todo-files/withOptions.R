#' Evaluate an expression with temporarily set options.
#'
#' @param opts [\code{named vector}]\cr
#'   Named vector of options to set.
#' @param expr [\code{expression}]\cr
#'   Expression to evaluate.
#' @return Evaluated expression.
#' @export
#' @examples
#'  withOptions(list(foo = 10), getOption("foo"))
withOptions = function(opts, expr) {
  setOptions = function(opts) do.call(options, opts)
  if (!isProperlyNamed(opts))
    stop("Argument 'opts' must be properly named")
  if (!is.list(opts))
    opts = as.list(opts)
  old = do.call(options, as.list(names(opts)))
  on.exit(setOptions(old))
  setOptions(opts)
  expr
}
