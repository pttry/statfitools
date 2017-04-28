#' Make syntactically valid names
#'
#' Try to make more convinient names than \code{\link{make.names}}.
#'
#'  Modifications:
#'  * replace some umlaut mark characters with a similar characters without
#'  * a space and punctuations replaced with "_"
#'  * remove duplicated, leading and trailing "_"
#'  * finnish with \code{\link{make.names}}
#'
#' @param x a character vector (of factor) with names to change
#' @param to_lower a locigal whether to turn names to lower case.
#' @param ... Arguments passed to \code{\link{make.names}}.
#' @return a character vector (or factor).
#' @export
#' @examples
#'   make_names(c("M\u00E4\u00E4r\u00E4", "Regional code"))
#'   make_names("Hello, world!")

make_names <- function (x, to_lower = FALSE, ...) {

  if (is.factor(x)){
    levels(x) <- make_names(levels(x))
    return(x)
  } else {
    patt <- c("\u00C4", "\u00E4", "\u00D6", "\u00F6", "\u00C5", "\u00E5", " ")
    repl <- c("A", "a", "O", "o", "A", "a", "_")

    # more general could be iconv(x, to="ASCII//TRANSLIT"), but TRANSLIT could produce extra characters

    for (i in seq_along(patt)){
      x <- gsub(patt[i], repl[i], x)
    }
    # remove punctuations
    x <- gsub("[[:punct:]]", "_", x)
    # extra _
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)

    x <- make.names(x, ...)

    if (to_lower) x <- tolower(x)

    x
  }

}

#' Make syntactically valid names and reuturn an object
#'
#' Apply \code{\link{make_names}} with \code{unique = TRUE}
#' to names of the object and return the object.
#'
#' @param x an object with names to change.
#' @param to_lower a locigal whether to turn names to lower case.
#' @return an object.
#' @export
#' @examples
#'    x <- data.frame("col 1!" = c(1,2), "col 2?" = c(2,4), check.names = FALSE)
#'    names(clean_names(x))
clean_names <- function(x, to_lower = FALSE){
  names(x) <- make_names(names(x), to_lower = to_lower, unique = TRUE)
  x
}


