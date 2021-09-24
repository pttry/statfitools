#' Turns codes to names or vice versa in Statistics Finland classifications
#'
#' Recode statfin classifications from numeric codes to names or vice versa.
#' \code{sf_code2name} and \code{sf_name2code} and shortcuts to main
#' \code{sf_recode} function.
#'
#' It is best to use column postions for \code{from} and \code{to}.
#' The first should be a numeric code and the second a name.
#'
#' @param x A vector for recoding.
#' @param from A column name or position for orginal classification
#'  in the classification data.frame. See \code{\link{sf_get_class}}.
#' @param to A column name or position for desidered classification
#'  in the classification data.frame. See \code{\link{sf_get_class}}.
#' @param ... futher parameters to \code{\link{sf_get_class}} to select classification.
#' @export


sf_recode <- function(x, from, to, ...){
  # cl <- sf_get_class(...)
  # y <- cl[[to]][match(x, cl[[from]])]
  # y
  .Defunct("statficlassifications::recode_region",
           msg = "To recode regions use functions from statficlassifications package")
}

#' @describeIn sf_recode From code to name.
#' @export
sf_code2name <- function(x, ...){
  # sf_recode(x, 1, 2, ...)
  .Defunct("statficlassifications::codes_to_names",
           msg = "To recode regions use functions from statficlassifications package")
}

#' @describeIn sf_recode From name to code.
#' @export
sf_name2code <- function(x, ...){
  # sf_recode(x, 2, 1, ...)
  .Defunct("statficlassifications::names_to_codes",
           msg = "To recode regions use functions from statficlassifications package")
}
