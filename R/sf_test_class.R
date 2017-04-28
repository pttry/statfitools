#' Test if classification used is Statistics Finland classification
#'
#' @param x avector to test for classification
#' @param col a column name from a classification data.frame to test against.
#' Names form Statistics Finland, see what code{\link{sf_get_class}} gives.
#' @param ... parameters to select a classification. Passed to \code{\link{sf_get_class}}.
#' @export
#' @return a locigal. And a warning message with levels that differ.

sf_test_class <- function(x, col, ...){
  #TODO guess col
  cl <- sf_get_class(...)[, col]

  y1 <- setdiff(c, x)
  y2 <- setdiff(x, cl)
  warning("Missing from x:\n", paste(y1, collapse = ", "),
          "\nMissing from classification:\n", paste(y2, collapse = ", "))

  y <- length(max(y1, y2)) == 0
  y
}

