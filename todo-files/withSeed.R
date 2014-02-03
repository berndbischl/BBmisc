#' Evaluate an expression using a specific seed.
#'
#' After evaluation, the RNG is restored to its previous state.
#'
#' @param seed [\code{integer(1)}]\cr
#'   Seed to set.
#' @param expr [\code{expression}]\cr
#'   Expression to evaluate.
#' @return Evaluated expression.
#' @export
#' @examples
#'  runif(1)
#'  before = .Random.seed
#'  withSeed(1, runif(1))
#'  stopifnot(before == .Random.seed)
withSeed = function(seed, expr) {
  seed = convertInteger(seed)
  checkArg(seed, "integer", len=1L, na.ok=FALSE, lower=0L)
  if (!exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE))
    runif(1L)
  old = get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
  on.exit(set.seed(old))
  set.seed(seed)
  expr
}
