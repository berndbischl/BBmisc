#' Require some packages.
#'
#' If some packages could not be loaded and \code{stop} is \code{TRUE}
#' the following exception is thrown:
#' \dQuote{For <why> please install the following packages: <missing packages>}.
#' If \code{why} is \code{NULL} the message is:
#' \dQuote{Please install the following packages: <missing packages>}.
#'
#' @param packs [\code{character}]\cr
#'   Names of packages.
#' @param why [\code{character(1)}]\cr
#'   Short string explaining why packages are required.
#'   Default is \code{NULL}.
#' @param stop [\code{logical(1)}]\cr
#'   Should an exception be thrown for missing packages?
#'   Default is \code{TRUE}.
#' @param suppress.warnings [\code{logical(1)}]\cr
#'   Should warnings be suppressed in the calls to \code{\link{require}}?
#'   Default is \code{FALSE}.
#' @param suppress.startup [\code{logical(1)}]\cr
#'   Should package startup messages be suppressed?
#'   Default is \code{TRUE}.
#' @param ... [any]\cr
#'   Passed on to \code{\link{require}}.
#' @return [\code{logical}]. Named logical vector describing which packages could be loaded.
#'   Same length as \code{packs}.
#' @export
#' @examples
#' requirePackages(c("BBmisc", "base"), why = "BBmisc example")
requirePackages = function(packs, why = NULL, stop = TRUE, suppress.warnings = FALSE, suppress.startup = TRUE, ...) {
  getSuppressor = function(suppress.warnings, suppress.startup) {
    if (suppress.warnings) {
      if (suppress.startup)
        return(function(expr) suppressPackageStartupMessages(suppressWarnings(expr)))
      return(suppressWarnings)
    }
    if (suppress.startup)
      return(suppressPackageStartupMessages)
    return(identity)
  }
  # strange do call construction beacause make check complained about ... context
  args = list(...)
  args$character.only = TRUE
  suppressor = getSuppressor(suppress.warnings, suppress.startup)
  packs.ok = sapply(packs, function(x) {
    args$package = x
    suppressor(do.call(require, args))
  })
  if(stop && !all(packs.ok)) {
    ps = collapse(packs[!packs.ok])
    if (is.null(why))
      stopf("Please install the following packages: %s", ps)
    else
      stopf("For %s please install the following packages: %s", why, ps)
  }
  return(packs.ok)
}
