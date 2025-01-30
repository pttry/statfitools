#' Translates code columns to name columns
#'
#' @param .data A data.frame or similar.
#' @param codes_names A named (column codes) list of named (codes) vectors (names).
#'        For example, from [px_code_name()].
#' @param to_name A vector of column names to be translated to names.
#'                Default (TRUE) is the names from `codes_names`.
#'                FALSE or NULL just pass `.data`.
#' @param name_suffix Suffix to add to name columns. Default is `"_name"`.
#' @param code_suffix Suffix to add to code columns. Default is `"_code"`.
#'
#' @export
#'
#' @return A modified data.frame or similar with `_name` and `_code` suffixes added as specified.
#'
#' @examples
#'
#' x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
#' cn <- list(a = c("a1" = "first", "a2" = "second"),
#'            b = c("b1" = "other", "b2" = "something"))
#' codes2names(x, cn)
#' codes2names(x, cn, to_name = "a", name_suffix = "_label", code_suffix = "_identifier")
codes2names <- function(.data, codes_names, to_name = TRUE,
                        name_suffix = "_name", code_suffix = "_code") {

  if (is.null(to_name)) return(.data)

  if (is.character(to_name)) {
    to_name <- to_name
  } else if (isTRUE(to_name)) {
    to_name <- intersect(names(codes_names), names(.data))  # Only keep columns that exist in data
  } else if (isFALSE(to_name)) {
    return(.data)
  }

  if (name_suffix == "") stop("name_suffix cannot be empty")

  # Ensure to_name only includes columns that exist in .data
  valid_to_name <- intersect(to_name, names(.data))

  # Ensure codes_names only includes keys present in .data
  valid_codes_names <- codes_names[names(codes_names) %in% valid_to_name]

  if (length(valid_to_name) == 0) return(.data)  # If no valid columns, return unchanged

  # Add name columns with the specified suffix
  .data <- dplyr::mutate(
    .data,
    across(
      any_of(valid_to_name) & (where(is.character) | where(is.factor)),
      ~ factor(
        .x,
        levels = names(valid_codes_names[[cur_column()]]),
        labels = valid_codes_names[[cur_column()]]
      ),
      .names = paste0("{.col}", name_suffix)
    )
  )

  # Rename original code columns with the specified suffix
  if (code_suffix != "") {
    .data <- dplyr::rename_with(
      .data,
      .cols = any_of(valid_to_name) & (where(is.character) | where(is.factor)),
      ~ paste0(.x, code_suffix)
    )
  }

  # Ensure all code columns are factors
  .data <- dplyr::mutate(
    .data,
    across(
      any_of(paste0(valid_to_name, code_suffix)) & (where(is.character) | where(is.factor)),
      ~ forcats::as_factor(.x)
    )
  )

  .data
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
