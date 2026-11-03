# BBmisc

BBmisc is an older package that predates the shared conventions in `AGENTS.md`.
Its code has not been migrated to that style yet.
Where the rules below conflict with `AGENTS.md`, these rules win.

## Code style

* Match the style of the surrounding code instead of the code style section in `AGENTS.md`.
* Do not reformat or restyle code you are not otherwise changing.
  The Air formatter, format on save, and lintr are disabled for this package on purpose.
* Function names use `camelCase` (e.g. `convertToShortString()`), arguments use `dot.case` (e.g. `num.format`).
  Do not rename existing functions or arguments; they are part of the public API.
* Use `=` for assignment, as most of the existing code already does.
* Do not add `cli`, `R6`, or `mlr3misc` as dependencies.
  Use base R and the existing helpers in the package (e.g. `stopf()`, `catf()`, `collapse()`).
* Use `checkmate` for argument checks, as existing code does.

## C code

* C sources live in `src/` and are registered in `src/init.c`.
  Match the existing indentation of the file you edit; clang-format is not applied automatically.

## Documentation

* Follow the existing roxygen style: `@title`, `@description`, and parameter types written as
  `#' @param x [\code{integer(1)}]\cr` followed by an indented description.
* There is no `README.Rmd`, `_pkgdown.yml`, `man-roxygen/`, or `R/bibentries.R`.
  Ignore the corresponding instructions in `AGENTS.md`.

## Tests

* Tests use testthat 2nd edition style (`context()` at the top of the file).
  Keep that style in existing files.

## NEWS.md

* Bullets in the current section use a Conventional Commits-style prefix, e.g. `* fix: ...`, `* docs: ...`.
