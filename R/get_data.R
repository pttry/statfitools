#' Get data from statfi
#'
#' Downloads data from statfi and cleans it.
#'
#' @param url A pxweb object or url that can be coherced to a pxweb object
#' @param query A json string, json file or list object that can be coherced to a pxweb_query object.
#' @param names Whether to add columns for names. "all", "none" or vector of variable names.
#'
#' @import pxweb
#' @import dplyr
#' @export
#'
#' @examples
#'   url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/tyti/statfin_tyti_pxt_11pk.px/"
#'   query <-
#'     list("Vuosi"=c("2010","2011"),
#'          "Sukupuoli"=c("SSS","1","2"),
#'          "Tiedot"=c("*"))
#'
#'   pp <- statfi_get_data(url, query)
#'   pp <- statfi_get_data(url, query, names = "none")
#'   pp <- statfi_get_data(url, query, names = c("Sukupuoli"))
#'
#'   url <- "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/tyti/statfin_tyti_pxt_135y.px/"
#'   query <-
#'    list("Kuukausi"=c("*"),
#'         "Sukupuoli"=c("SSS"),
#'         "Ik\U00E4luokka" = c("15-64"),
#'         "Tiedot" = c("*"))
#'
#'   pp2 <- statfi_get_data(url, query)

statfi_get_data <- function(url, query, names = "none"){

  message("Downloading: ", url)

  px_data <- pxweb::pxweb_get(url = url, query = query)

  codes_names <- px_code_name(px_data)


  # columns to name
  if (names == "all") {
    to_name <- names(codes_names)
  } else if (names == "none") {
    to_name <- NULL
  } else {
    to_name <- names
  }


  px_df <- as.data.frame(px_data, column.name.type = "code",
                         variable.value.type = "code") %>%
    # All longer
    tidyr::pivot_longer(where(is.numeric),
                        names_to = setdiff(names(codes_names), names(.)),
                        values_to = "values") %>%
    statfitools::clean_times2() %>%
    codes2names(codes_names, to_name) %>%
    dplyr::mutate(across(where(is.character), ~forcats::as_factor(.x))) %>%
    statfitools::clean_names() %>%
    relocate(time) %>%
    relocate(values, .after = last_col()) %>%
    droplevels()

  codes_names <- statfitools::clean_names(codes_names)
  attributes(px_df)$codes_names <- codes_names


  px_df
}

utils::globalVariables(c("time", "values", "where"))
