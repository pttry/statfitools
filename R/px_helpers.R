#' Translates code columns to name columns
#'
#' @param .data A data.frame or similar.
#' @param codes_names A named (column codes) list of named (codes) vectors (names).
#'        For example from [px_code_name()].
#' @param to_name A columns to name. Default are names from [codes_names] list.
#'
#' @export
#'
#' @return A modified data.frame or similar.
#'
#' @examples
#'
#' x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
#' cn <- list(a = c("a1" = "first", "a2" = "second"),
#'            b = c("b1" = "other", "b2" = "something"))
#' codes2names(x, cn)
#' codes2names(x, cn, "a")

codes2names <- function(.data, codes_names, to_name = names(codes_names)){

  if (is.null(to_name)) return(.data)

  .data <- dplyr::mutate(.data, across(any_of(to_name) & where(is.character),
                                       ~factor(.x,
                                               levels = names(codes_names[[cur_column()]]),
                                               labels = codes_names[[cur_column()]]),
                                       .names = "{.col}_name"))

  .data <- dplyr::rename_with(.data, .cols = any_of(to_name) & where(is.character), ~paste0(.x, "_code"))
  dplyr::mutate(.data, across(contains("_code"), ~forcats::as_factor(.x)))
}


#' Get code name mapping from pxweb_data
#'
#' @param px_data A pxweb_data object.
#'
#' @export
#'
#' @return A named (column codes) list of named (codes) vectors (names).
#'
px_code_name <- function(px_data){
  purrr::map(rlang::set_names(px_data$pxweb_metadata$variables,
                              sapply(px_data$pxweb_metadata$variables, "[[", "code")),
             ~rlang::set_names(.x$valueTexts, .x$values))
}
