#' @title Capitalize strings in a vector
#'
#' @description
#' Capitalise first word or all words of a character vector.
#' Lower back of vector element or word, respectively.
#' 
#' @param x [\code{character(n)}]\cr
#'   Vector of character elements to capitalize.
#' @param all.words [\code{logical(1)}]\cr
#'   If \code{TRUE} all words of each vector element are capitalized.
#'   \code{FALSE} capitalizes the first word of each vector element.
#' @param lower.back [\code{logical(1)}]\cr
#'   \code{TRUE} lowers the back of each word or vector element (depends on \code{all.words}).
#' @return Capitalized vector: [\code{character(n)}].
#' @export
#' @examples
#' capitalizeStrings(c("the taIl", "wags The dOg", "That looks fuNny!"))
#' capitalizeStrings(c("the taIl", "wags The dOg", "That looks fuNny!")
#' , all.words = TRUE, lower.back = TRUE)
capitalizeStrings = function(x, all.words = FALSE, lower.back = FALSE) {
  assertCharacter(x)
  assertLogical(all.words, any.missing = FALSE, len = 1L)
  assertLogical(lower.back, any.missing = FALSE, len = 1L)
  
  if (all.words) {
    pattern = "([[:alnum:]])([[:alnum:]]*)"
    replacement = "\\U\\1"
    if (lower.back) {
      replacement = paste0(replacement, "\\L\\2")
    } else {
      replacement = paste0(replacement, "\\E\\2")
    }
  } else {
    pattern = "^([[:alnum:]])"
    replacement = "\\U\\1"
    if (lower.back) {
      pattern = paste0(pattern, "(.*)")
      replacement = paste0(replacement, "\\L\\2")
    }
  }
  
  return(gsub(pattern, replacement, x, perl = TRUE))
}
