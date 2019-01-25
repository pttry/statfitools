#' Convert Statfin time variable(s) to numeric or date
#'
#' @param x A data.frame.
#' @param time_format "num" for numeric (default for yearly) and
#'   "date" for \code{\link{Date}} (default otherwise)
#' @param year_col A column name for year
#' @param sub_year_col A column name for quarter or mounth.
#'   If \code{NULL} (default) tries to guess.
#' @param agg_time sub_year_col might contain also aggregate values, like
#'   yearly sums. They are exluded based on order of factor level or occurence
#'   in data. Default is that nothing is exluded.
#' @export
#' @return A data.frame
#' @examples
#' public_debt2 <- clean_times(public_debt)
#' str(public_debt2)
#' str(clean_times(output_ind, time_format = "date"))
#' employment_q2 <- clean_times(x = employment_q, sub_year_col = "Ajanjakso", agg_time = "Vuosikeskiarvo")


clean_times <- function(x, time_format = NULL,
                        year_col = "Vuosi", sub_year_col = NULL,
                        agg_time = NULL){
  # guess sub year
  if (is.null(sub_year_col)) {
    sub_years <- c("Nelj\u00e4nnesvuosi", "Neljannesvuosi", "Kuukausi")
    sub_year_col <- sub_years[sub_years %in% names(x)]
  }

  # Only yearly
  if (length(sub_year_col) == 0){
    if (is.null(time_format) || time_format == "num"){
      time <- readr::parse_number(as.character(x[[year_col]]))
    } else if (time_format == "date"){
      time <- as.Date(
        paste0(readr::parse_number(as.character(x[[year_col]])), "-01-01"))
    } else {
      stop("Unknown time_format ", time_format)
    }

    x <- rename(x, time = year_col)
    x$time <- time
    return(x)

  # mounthly or quertely
  } else {


    # get months or quarters
    subs <- unique(x[[sub_year_col]])

    # remove aggregate time value. Quarterly and monthly allowed
    subs <- setdiff(subs, agg_time)
    if (!(length(subs) %in% c(4,12))) {
      stop("Only quarterly and monthly data is cleaned. Check sub_year_col and agg_time")
    }

    x <- droplevels(x[(x[[sub_year_col]] %in% subs),])


    mounths <- seq.int(1, by = 12/length(subs), length.out = length(subs))

    if (is.null(time_format) || time_format == "date"){
      time <- as.Date(
        paste0(readr::parse_number(as.character(x[[year_col]])), "-",
               mounths[match(x[[sub_year_col]], subs)] , "-1"))

    } else if (time_format == "num") {
      time <- readr::parse_number(as.character(x[[year_col]])) +
        (match(x[[sub_year_col]], subs) - 1) * 1/length(subs)
    } else {
      stop("Unknown time_format ", time_format)
    }

    x[, sub_year_col] <- NULL
    x <- rename(x, time = year_col)
    x$time <- time
    return(x)
  }


}
