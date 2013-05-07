#' Is one / are several files a directory?
#'
#' If a file does not exist, \code{FALSE} is returned.
#'
#' @param ... [\code{character(1)}]\cr
#'   File names, all strings.
#' @return [\code{logical}].
#' @export
#' @examples
#' print(isDirectory(tempdir()))
#' print(isDirectory(tempfile()))
isDirectory = function(...) {
  # FIXME: file_test("-d", ...) might be faster?
  x = file.info(...)$isdir
  !is.na(x) & x
}


#' Is one / are several directories empty?
#'
#' If file does not exist or is not a directory, \code{FALSE} is returned.
#'
#' @param ... [\code{character(1)}]\cr
#'   Directory names, all strings.
#' @return [\code{logical}].
#' @export
#' @examples
#' print(isEmptyDirectory(tempdir()))
#' print(isEmptyDirectory(tempfile()))
isEmptyDirectory = function(...) {
  vapply(list(...), FUN.VALUE=TRUE, FUN=function(x) {
    isDirectory(x) && length(list.files(x, all.files=TRUE, include.dirs=TRUE)) == 2L
  })
}
