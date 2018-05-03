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
