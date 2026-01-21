#' @title Require some packages
#'
#' @description
#' Packages are loaded either via \code{\link{requireNamespace}} or \code{\link{require}}.
#'
#' If some packages could not be loaded and \code{stop} is \code{TRUE}
#' the following exception is thrown:
#' \dQuote{For <why> please install the following packages: <missing packages>}.
#' If \code{why} is \code{NULL} the message is:
#' \dQuote{Please install the following packages: <missing packages>}.
#'
#' @param packs [\code{character}]\cr
#'   Names of packages.
#'   If a package name is prefixed with \dQuote{!}, it will be attached using \code{\link[base]{require}}.
#'   If a package name is prefixed with \dQuote{_}, its namespace will be loaded using \code{\link[base]{requireNamespace}}.
#'   If there is no prefix, argument \code{default.method} determines how to deal with package loading.
#' @param min.versions [\code{character}]\cr
#'   A char vector specifying required minimal version numbers for a subset of packages in \code{packs}.
#'   Must be named and all names must be in \code{packs}.
#'   The only exception is when \code{packs} is only a single string, then you are allowed to pass
#'   an unnamed version string here.
#'   Default is \code{NULL}, meaning no special version requirements
#' @param why [\code{character(1)}]\cr
#'   Short string explaining why packages are required.
#'   Default is an empty string.
#' @param stop [\code{logical(1)}]\cr
#'   Should an exception be thrown for missing packages?
#'   Default is \code{TRUE}.
#' @param suppress.warnings [\code{logical(1)}]\cr
#'   Should warnings be supressed while requiring?
#'   Default is \code{FALSE}.
#' @param default.method [\code{character(1)}]\cr
#'   If the packages are not explicitly prefixed with \dQuote{!} or \dQuote{_},
#'   this arguments determines the default. Possible values are \dQuote{attach} and
#'   \dQuote{load}.
#'   Note that the default is \dQuote{attach}, but this might/will change in a future version, so
#'   please make sure to always explicitly set this.
#' @return [\code{logical}]. Named logical vector describing which packages could be loaded (with required version).
#'   Same length as \code{packs}.
#' @export
#' @examples
#' requirePackages(c("BBmisc", "base"), why = "BBmisc example")
requirePackages = function(packs, min.versions = NULL, why = "", stop = TRUE, suppress.warnings = FALSE, default.method = "attach") {
  assertCharacter(packs, any.missing = FALSE)
  if (!is.null(min.versions)) {
    assertCharacter(min.versions)
    if (length(packs) == 1L && length(min.versions) == 1L && is.null(names(min.versions)))
      names(min.versions) = packs
    else
      assertSubset(names(min.versions), packs)
  }
  assertFlag(stop)
  assertFlag(suppress.warnings)
  assertChoice(default.method, choices = c("load", "attach"))

  char = substr(packs, 1L, 1L)
  force.attach = (char == "!")
  force.load = (char == "_")
  ns.only = if (default.method == "load") !force.attach else force.load
  packs = substr(packs, 1L + (force.load | force.attach), nchar(packs))
  suppressor = if (suppress.warnings) suppressWarnings else identity

  packs.ok = unlist(Map(function(pack, ns.only) {
    if (ns.only) {
      suppressor(requireNamespace(pack, quietly = TRUE))
    } else {
      suppressor(require(pack, character.only = TRUE))
    }
  }, pack = packs, ns.only = ns.only))

  if (stop && !all(packs.ok)) {
    ps = collapse(packs[!packs.ok])
    if (nzchar(why))
      stopf("For %s please install the following packages: %s", why, ps)
    else
      stopf("Please install the following packages: %s", ps)
  }

  if (!is.null(min.versions)) {
    packs.wrong.version = character(0L)
    for (j in seq_along(min.versions)) {
      pn = names(min.versions)[j]
      mv = min.versions[j]
      if (packageVersion(pn) < mv) {
        packs.ok[pn] = FALSE
        packs.wrong.version = c(packs.wrong.version, sprintf("%s >= %s", pn, mv))
      }
    }
    if (stop && length(packs.wrong.version) > 0L) {
      ps = collapse(packs.wrong.version)
      if (nzchar(why))
        stopf("For %s the package version requirements are not fulfilled: %s", why, ps)
      else
        stopf("The package version requirements are not fulfilled: %s", ps)
    }
  }

  return(packs.ok)
}
