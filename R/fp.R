#' Calculate fixed price series
#'
#' For a yearly data calculates a serie in fixed prices at base years
#' price level for series in current prices and previous year's prices.
#'
#' @param cp a current price variable.
#' @param pp a previous year price variable.
#' @param time a variable for years.
#' @param year a numeric for a base year.
#'
#' @export
#' @return an vector.
#'
#' @examples
#' fp(cp = c(1, 2, 3), pp = c(NA, NA, NA), time = c(2014,2015,2016), year = 2015)
#' fp(cp = c(1, 2, 3), pp = c(1, 2, 4), time = c(2014,2015,2016), year = 2015)
#' fp(cp = c(1, 2, 3), pp = c(NA, 2, 4), time = c(2014,2015,2016), year = 2015)
#' fp(cp = c(NA, 2, 3), pp = c(NA, NA, 4), time = c(2014,2015,2016), year = 2015)
#'
fp <- function(cp, pp, time, year){
  # index to remove leading NA
  non_na_ind <- cumsum(!is.na(cp)) != 0
  cp <- cp[non_na_ind]
  pp <- pp[non_na_ind]
  time <- time[non_na_ind]
  ind <- pp/lag(cp)
  ind[1] <- 1
  ind <- cumprod(ind)
  y0 <- cp[time == year] * ind / ind[time == year]
  y <- rep_len(NA, length(non_na_ind))
  y[non_na_ind] <- y0
  y
}


#' Calculate previous years price series
#'
#' For a yearly data calculates a serie in previous year's prices
#' from fixed and current price series.
#'
#' @param cp a current price variable.
#' @param fp a fixed year price variable.
#' @param time a variable for years.
#'
#' @export
#' @return an vector
#'
#' @examples
#'   pp(c(100, 120), c(100, 110), time = c(1,2))
#'
pp <- function(cp, fp, time){
  y <- lag(cp, order_by = time) * (fp/lag(fp, order_by = time))
  y
}
