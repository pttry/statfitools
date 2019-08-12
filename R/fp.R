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
#' @return an vector
#'
#'
fp <- function(cp, pp, time, year){
  ind <- pp/lag(cp)
  ind <- if_else(is.na(ind), 1, ind)
  ind <- cumprod(ind)
  y <- cp[time == year] * ind / ind[time == year]
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
