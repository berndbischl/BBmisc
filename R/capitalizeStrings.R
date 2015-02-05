#' @title Capitalize strings in a vector
#'
#' @description
#' Capitalise first word or all words of a character vector.
#' Lower back of vector element or word, respectively.
#' 
#' @author Dirk Surmann
#'
#' @param string [\code{character(n)}]\cr
#'   Vector of character elements to capitalize.
#' @param allWords [\code{logical(1)}]\cr
#'   If \code{TRUE} all words of each vector element are capitalized.
#'   \code{FALSE} capitalizes the first word of each vector element.
#' @param lowerBack [\code{logical(1)}]\cr
#'   \code{TRUE} lowers the back of each word or vector element (depends on \code{allWords}).
#' @return Capitalized vector: [\code{character(n)}].
#' @export
#' @examples
#' capitalizeStrings(c("the taIl", "wags The dOg", "That looks fuNny!"))
#' capitalizeStrings(c("the taIl", "wags The dOg", "That looks fuNny!"), allWords=TRUE, lowerBack=TRUE)
capitalizeStrings <- function(string, allWords=FALSE, lowerBack=FALSE) {
  checkArg(string, "character")
  checkArg(allWords, "logical", len=1, na.ok=FALSE)
  checkArg(lowerBack, "logical", len=1, na.ok=FALSE)
  
  if (allWords) {
    pattern <- "([[:alnum:]])([[:alnum:]]*)"
    replacement <- "\\U\\1"
    if (lowerBack) {
      replacement <- paste0(replacement, "\\L\\2")
    } else {
      replacement <- paste0(replacement, "\\E\\2")
    }
  } else {
    pattern <- "^([[:alnum:]])"
    replacement <- "\\U\\1"
    if (lowerBack) {
      pattern <- paste0(pattern, "(.*)")
      replacement <- paste0(replacement, "\\L\\2")
    }
  }
  
  gsub(pattern, replacement, string, perl=TRUE)
}
