#' Convert Statfin time variable to Date.
#'
#'
#' @param x A data.frame
#' @param time_col  column in x containing the time variable
#' @param agg_time
#'
#' @return A data.frame
#' @export
#'
#' @examples
#'
#' data_m <- data.frame(values = rnorm(24),
#'                      Kuukausi = as.vector(sapply(as.character(2016:2017),
#'                                                  paste0,
#'                                                  paste0("M", c(paste0("0", 1:9), as.character(10:12))))))
#' clean_times2(data_m)
#'
#' data_q <- data.frame(values = rnorm(12),
#'                      Vuosineljännes = as.vector(sapply(as.character(2016:2018),
#'                                                        paste0,
#'                                                        paste0("Q", as.character(1:4)))))
#' clean_times2(data_q)
#'
#' data_y <- data.frame(values = rnorm(10),
#'                      Vuosi = as.character(2010:2019))
#' clean_times2(data_y)
#'
clean_times2 <- function (x, time_col = NULL, agg_time = NULL)
{

  if(!is.data.frame(x)) {stop("Input not data.frame!")}

  if(is.null(time_col)) {
    potential_time_cols <- c("kuukausi", "Kuukausi", "Vuosineljannes", "vuosineljannes",
                             "Vuosineljännes", "vuosineljännes", "vuosi", "Vuosi")
    time_col <- potential_time_cols[potential_time_cols %in% names(x)]
  }
  if(length(time_col) == 0) {
    stop("Time column not automatically found. Please assign time column to time_col.")
  }

  year_col <-  substring(x[[time_col]],1,4)
  sub_year_col <- substring(x[[time_col]], 6,7)

  if (nchar(paste(sub_year_col, collapse = "")) == 0) {
    time <- as.Date(paste0(as.character(year_col), "-01-01"))
    x$time <- time
    x[[time_col]] <- NULL
    return(x)
  }
  else {
    subs <- unique(sub_year_col)
    subs <- setdiff(subs, agg_time)
    if (!(length(subs) %in% c(4, 12))) {
      stop("Only yearly, quarterly and monthly data is cleaned. Check sub_year_col and agg_time")
    }
    x <- droplevels(x[(sub_year_col %in% subs), ])
    months <- seq.int(1, by = 12/length(subs), length.out = length(subs))
    time <- as.Date(paste0(as.character(year_col),
                           "-", months[match(sub_year_col, subs)],
                           "-1"))
    x$time <- time
    x[[time_col]] <- NULL
    return(x)
  }
}
