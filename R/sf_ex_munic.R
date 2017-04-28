#' Download data.frame for abolished municipalities
#'
#' Download data.frame for abolished municipalities
#'
#' @param url a url for text file in Statistics Finland web page.
#' @return data.frame with columns
#'    old_code, old_name, "data", new_code, new_name.
#' @export
#' @examples
#'    k <- sf_get_ex_munic()
sf_get_ex_munic <- function(
  url =
    "http://www.tilastokeskus.fi/meta/luokitukset/_linkki/lakkautetut_kunnat_aakkosissa_15.txt")
{
  # check if current url
  if (!grepl(substr(Sys.Date(), 3,4), url)) warning(
    "The url for sf_get_ex_munic() might be out of date.")

  z <- suppressWarnings(
    readr::read_tsv(file = url, na = "-", col_types = "ccccc",
                       locale = readr::locale(encoding = "latin1")))
  y <- z[, c(1:5)]
  names(y) <- c("old_code", "old_name", "date", "new_code", "new_name")
  y$date <- as.Date(y$date, format = "%d.%m.%Y")
  y
}


#' Replace codes of abolished municipalities with present codes
#'
#' @param x a vector of municipal codes to replace.
#' @param year a year which codes should be used for new codes. Changes up to
#'    1.1. at that year.
#'    `NULL` (the default) uses
#'    the most up to date codes is mcodes.
#' @param mcodes a data.frame in form given by \code{\link{sf_get_ex_munic}}.
#' @export

sf_recode_ex_munic <- function(x, year = NULL, mcodes = sf_get_ex_munic()){
  # discard name changes
  mcodes <- subset(mcodes, !(old_code == new_code))

  if (!is.null(year)){
    mcodes  <- subset(mcodes, date < as.Date(paste(year, 1, 2, sep = "-")))
  }
  # there could be several changes for municipalities
  y <- as.numeric(x)
  if (any(is.na(y))){
    x[is.na(y)] <- "000"
    warning(unique(x[is.na(y)]), " replaced with 0")
  }
  while (any(x %in% mcodes$old_code)){
    x <- plyr::mapvalues(x, mcodes$old_code, mcodes$new_code, warn_missing = FALSE)
  }
  x
}
