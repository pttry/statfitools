#' Extract a code component
#'
#' This uses a regular expression to get a code from beging of a character
#' string. Useful to extract names from code-name variables.
#'
#'
#'
#' @param x A character vector (or a factor).
#' @param numbers_as_numeric A locigal. Whether to try to convert a code to
#'   a numeric.
#' @return  A character vector (or a factor).
#' @export
#' @seealso \code{\link{extract_name}}
#'
#' @examples
#'  extract_code("508 Mantta-Vilppula")
#'  extract_code("508 Mantta-Vilppula", numbers_as_numeric = FALSE)

extract_code <- function (x, numbers_as_numeric = TRUE) {
  if (is.factor(x)){
    levels(x) <- extract_code(levels(x), numbers_as_numeric = numbers_as_numeric)
    return(x)
  } else{
    y <- gsub(" .*", "", x)
    if (numbers_as_numeric) {
      num <- suppressWarnings(as.numeric(y))
      if (!any(is.na(num))) y <- num
      }
    y
  }
}


#' Extract only a non-code component
#'
#' This uses a regular expression to strip a code from beging of a character
#' string. Useful to extract names from code-name variables.
#'
#' @param x A character vector (or a factor).
#' @return  A character vector (or a factor).
#' @export
#' @seealso \code{\link{extract_code}}
#'
#' @examples
#'  extract_name("S1311 Valtionhallinto")

extract_name <- function (x) {
  if (is.factor(x)){
    levels(x) <- extract_name(levels(x))
    return(x)
  } else {
    y <- gsub("^[[:alnum:][:punct:]]+ +", "", x)
    y
  }
}

