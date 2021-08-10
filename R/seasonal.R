#' Easy and error-resistant wrap-up for seasonal adjustment
#'
#' Seasonal adjustment using \code{\link[seasonal]{seasonal-package}} and
#' \code{\link[RJDemetra]{RJDemetra-package}}. Functions
#' take numeric and date vector and return numeric vector. If adjustment
#' end in error, NA vector returned with warning.
#'
#' Intended specially to use in pipe with \code{\link[dplyr]{mutate}}.
#'
#' @param x a numeric vector to seasonal adjust.
#' @param time a date vector.
#' @param method a method for seasonal adjustment. "tramoseats" or "x13" from the RJDemetra or
#'        "seas" for x13 from the seasonal package.
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
#' y <- sa_series(x = AirPassengers, time = zoo::as.Date(AirPassengers))
#' yy <- sa_series(x = AirPassengers, time = zoo::as.Date(AirPassengers), method = "tramoseats")
#' yyy <- trend_series(x = AirPassengers, time = zoo::as.Date(AirPassengers), method = "tramoseats")

seasonal_adj <- function(x, time, series = "sa", outlier.types = "ao", ...){

  freq <- 1/lubridate:: time_length(
    lubridate::interval(time[1], time[2]), unit = "year")
  freq <- round(freq)
  x_ts <- stats::ts(x, lubridate::year(time[1]), frequency = freq)

  y <- try(seasonal::seas(x_ts, ..., outlier.types = outlier.types,
                          na.action = stats::na.exclude), silent = TRUE)

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

#' @describeIn seasonal_adj Seasonal adjustment with RJDemetra
#'
#' @import RJDemetra
#' @export
#'
#' @examples
#'   #' y <- demetra_adj(x = AirPassengers, time = zoo::as.Date(AirPassengers))
#'


demetra_adj <- function(x, time, method = "tramoseats", series = "sa", ...){

  freq <- 1/lubridate:: time_length(
    lubridate::interval(time[1], time[2]), unit = "year")
  freq <- round(freq)
  x_ts <- stats::ts(x, lubridate::year(time[1]), frequency = freq)

  if (method == "tramoseats") {
    y <- try(RJDemetra::jtramoseats(x_ts, ...), silent = TRUE)
  } else if (method == "X13"){
    y <- try(RJDemetra::jx13(x_ts, ...), silent = TRUE)
  }

  #If fails..
  if (inherits(y, "try-error")){
    zz <- rep_len(NA, length.out = length(x))
    warning("RJDmetra, ", method,  " failed")
    return(zz)
  }

  # if ok..
  if (series == "sa"){
    z <- c(RJDemetra::get_indicators(y, "sa")[[1]])
  } else if (series == "trend") {
    z <- c(RJDemetra::get_indicators(y, "t")[[1]])
  } else {
    stop("Unknown series spesicication")
  }

  if (is.null(z)) {
    z <- rep_len(NA, length.out = length(x))
    warning("RJDmetra, ", method,  " failed")
  }

  z
}


#' @describeIn seasonal_adj seasonal adjusted series
#' @export
#'
sa_series <- function(x, time, method = "seas", ...){
  if (method == "seas") {
    y <- seasonal_adj(x, time, ..., series = "sa")
  } else if (method %in% c("tramoseats", "x13")){
    y <- demetra_adj(x, time, method = method, ..., series = "sa")
  }
  y
}

#' @describeIn seasonal_adj trend of seasonal adjusted series
#' @export

trend_series <- function(x, time, method = "seas", ...){
  if (method == "seas") {
    y <- seasonal_adj(x, time, ..., series = "trend")
  } else if (method %in% c("tramoseats", "x13")){
    y <- demetra_adj(x, time, method = method, ..., series = "trend")
  }
  y
}
