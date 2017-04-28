#' Download statistical classifications from the Statistics Finland.
#'
#' Download a data.frame of classifications from the Statistics Finland webpage.
#' The data.frame includes codes and names for a classifications.
#'
#' For all available classifications see
#' \url{http://tilastokeskus.fi/meta/luokitukset/index_en.html} (for english)
#' and \url{http://tilastokeskus.fi/meta/luokitukset/} (for finnish).
#'
#' Some of available classification include:
#'
#' Regional
#' \itemize{
#'   \item kunta
#'   \item seutukunta
#'   \item maakunta
#'   \item kuntaryhmitys
#'   \item tyossakayntial
#' }
#'
#' Economic
#' \itemize{
#'   \item toimiala
#'   \item cpa
#' }
#'
#'
#' @param class a a name of the classification.
#' @param year a year of the classification.
#' @param lang a language code. Available "fi", "en", "sv".
#' @param as_factors a logical. Should columns be converted to factors. The
#'        default is from \code{\link{options}}("stringsAsFactors").
#' @return a data.frame.
#' @family sf_class
#' @export
#' @examples
#' x <- sf_get_class(class = "kunta", year = 2016)
#' y <- sf_get_class("seutukunta", 2013)
#' z <- sf_get_class("maakunta", 2016)
#' t <- sf_get_class("toimiala", 2008, lang = "en")
sf_get_class <- function(class, year, lang = "fi",
                         as_factors = default.stringsAsFactors()){
  lang <- if(lang == "fi") "" else paste0("_", lang)
  base_url <- "http://tilastokeskus.fi/meta/luokitukset/"
  tk_url <- paste0(base_url, class, "/001-", year, "/tekstitiedosto", lang, ".txt")
  z <- suppressMessages(readr::read_tsv(file = tk_url, na = "-", skip = 3,
                       col_types = NULL,
                       locale = readr::locale(encoding = "latin1")))

  if (as_factors) {
    z[] <- lapply(z, function(x) factor(x, levels = unique(x)))
  }

  z
}


#' Download data.frame of Statistics Finland classification key.
#'
#' Download data.frame of classification key from Statistics Finland. At least
#' regional classifications other that administrative. For administrative
#' region keys see \code{\link{sf_get_reg_key}}.
#'
#' Versions for all years are not available and Statistical Finland might also
#' remove older versions.
#'
#' @param year A year of codes.
#' @param classification a regional classification.
#' Allowed at least: "kuntaryhmitys", "tyossakayntial".
#' @return A data.frame.
#' @family sf_class
#' @export
#' @examples
#' x <- sf_get_class_key("kuntaryhmitys", 2016)
#' y <- sf_get_class_key("tyossakayntial", 2016)
sf_get_class_key <- function(classification, year){
  .Deprecated("sf_get_class")
  base_url <- "http://tilastokeskus.fi/meta/luokitukset/"
  end_url <- "/tekstitiedosto.txt"
  tk_url <- paste0(base_url, classification, "/001-", year, end_url)
  z <- utils::read.delim2(file = tk_url, na.strings = "-", skip = 3)
  z
}

# "http://www.tilastokeskus.fi/meta/luokitukset/kunta/001-2016/kunta_mk_teksti.txt"
# http://www.tilastokeskus.fi/meta/luokitukset/kunta/001-2015/mk_teksti.txt

#' Download a data.frame of key to trasform from kunta to other classifications
#'
#' Download a data.frame of regional classifications keys from Statistics Finland.
#'
#' @param year A year of codes.
#' @param to_reg a regional classification to tranforms.
#' Allowed at least: "maakunta", "skunta".
#' @return A data.frame.
#' @export
#' @family sf_class
#' @examples
#' x <- sf_get_reg_key("maakunta", 2014)
#' y <- sf_get_reg_key(to_reg = "skunta", year = 2013)

sf_get_reg_key <- function(to_reg, year){
  .Deprecated("sf_get_reg_keytable")
  base_url <- "http://tilastokeskus.fi/meta/luokitukset/kunta/"
  mid_url <- "/luokitusavain_"
  end_url <- "_teksti.txt"
  tk_url <- paste0(base_url, "/001-", year, mid_url, to_reg, end_url)
  z <- utils::read.delim2(file = tk_url, na.strings = "-", skip = 3)

  z
}


#' Get regional classification table from Statistics Finland
#'
#' Get regional classification table for municipalities from Statistics Finland.
#' Table includes all municipality based regional classifications in Finland.
#' Classifications include Finnish, Swedish and English versions. The table is
#' from \url{}
#'
#' @param classes a name or vector of names of the regional classification.
#'        NULL (default) return all classifications. Names can be names in
#'        orginal table or valid names given by \code{\link{make_names}}.
#' @param url a url of the table.
#' @param as_factors a logical. Should columns be converted to factors. The
#'        default is from \code{\link{options}}("stringsAsFactors").
#' @export
#' @return a tibble with columns Knro (classification number of a municipality),
#'         Kunta (a name of a municipality) and colums for a code and a name of
#'         the \code{classes} classification.
#' @examples
#'   key_table <- sf_get_reg_keytable()
#'   key_table_mk <- sf_get_reg_keytable("Maakunta")
#'   key_table_mk_sk <- sf_get_reg_keytable(c("Maakunta", "Seutukunta"))
#'   key_table_mk_sk <- sf_get_reg_keytable(c("Maakunta", "Region"))


sf_get_reg_keytable <-
  function(classes = NULL,
           url = "http://www.tilastokeskus.fi/static/media/uploads/meta/luokitukset/kooste_2016_kaikki_kielet.xlsx",
           as_factors = default.stringsAsFactors()){
  # Download and prepare table
  xfile <- tempfile(fileext = ".xlsx")
  utils::download.file(url, xfile, mode = "wb")
  x <- readxl::read_excel(xfile)
  unlink(xfile)
  x <- clean_names(x)

  # select columns
  if (!is.null(classes)){
    name_cols <- match(make_names(classes), names(x))
    code_cols <- grep("koodi", names(x))
    cols <- unlist(lapply(name_cols, function(nc){
      cc <- max(code_cols[which(code_cols < nc)])
      c(cc, nc)
    }))
    x <- x[, c(1,2, unique(cols))]
  }

  # trim whitespace and turn to factor
  x[] <- lapply(x, trimws)
  if (as_factors) {
    x[] <- lapply(x, function(x) factor(x, levels = unique(x)))
  }
  x
}
