#' Easy and error-resistant wrap-up for seasonal adjustment
#'
#' Seasonal adjustment using \code{\link[seasonal]{seasonal-package}}. Functions
#' take numeric and date vector and return numeric vector. If adjustment
#' end in error, NA vector returned with warning.
#'
#' Intended specially to use in pipe with \code{\link[dplyr]{mutate}}.
#'
#' @param x a numeric vector to seasonal adjust.
#' @param time a date vector.
#' @param series a series to return. "sa" for seasonal adjusted and "trend"
#'        for trend.
#' @param outlier.types an outlier parameter passed to
#'        \code{\link[seasonal]{seas}}. Default "ao", only additive outliers
#'        differs from \code{seas} default.
#' @param ... parameters passed to \code{\link[seasonal]{seas}}.
#'
#' @export
#' @import dplyr
#' @examples
#' org <- AirPassengers %>%
#'   tibble(time = lubridate::date_decimal(as.numeric(time(.))), values = .)
#'
#' y <- sa_series(x = org$values, time = org$time)

seasonal_adj <- function(x, time, series = "sa", outlier.types = "ao", ...){

  freq <- 1/lubridate:: time_length(
    lubridate::interval(time[1], time[2]), unit = "year")
  freq <- round(freq)
  x_ts <- ts(x, lubridate::year(time[1]), frequency = freq)

  y <- try(seasonal::seas(x_ts, ..., outlier.types = outlier.types,
                          na.action = na.exclude), silent = TRUE)

  #If fails..
  if (inherits(y, "try-error")){
    zz <- rep_len(NA, length.out = length(x))
    warning("X-13 failed")
    return(zz)
  }

  # if ok..
  if (series == "sa"){
    z <- c(seasonal::final(y))
  } else if (series == "trend") {
    z <- c(seasonal::trend(y))
  }
  z
}



#' @describeIn seasonal_adj seasonal adjusted series
#' @export
#'
sa_series <- function(x, time, ...){
  y <- seasonal_adj(x, time, ..., series = "sa")
  y
}

#' @describeIn seasonal_adj trend of seasonal adjusted series
#' @export

trend_series <- function(x, time, ...){
  y <- seasonal_adj(x, time, ..., series = "trend")
  y
}
