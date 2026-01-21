#' @title A wrapper for \code{\link{sort}} to sort using the \dQuote{C} collating rules
#'
#' @description
#' Sorts using the C locale for consistent sorting across platforms.
#'
#' @param ...
#'   Options passed to sort.
#' @return See \code{\link{sort}}.
#' @export
lsort = function(...) {
  cur = Sys.getlocale("LC_COLLATE")
  if (cur != "C") {
    Sys.setlocale("LC_COLLATE", "C")
    on.exit(Sys.setlocale("LC_COLLATE", cur))
  }
  sort(...)
}
