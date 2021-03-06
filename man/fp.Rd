% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fp.R
\name{fp}
\alias{fp}
\title{Calculate fixed price series}
\usage{
fp(cp, pp, time, year)
}
\arguments{
\item{cp}{a current price variable.}

\item{pp}{a previous year price variable.}

\item{time}{a variable for years.}

\item{year}{a numeric for a base year.}
}
\value{
an vector.
}
\description{
For a yearly data calculates a serie in fixed prices at base years
price level for series in current prices and previous year's prices.
}
\examples{
fp(cp = c(1, 2, 3), pp = c(NA, NA, NA), time = c(2014,2015,2016), year = 2015)
fp(cp = c(1, 2, 3), pp = c(1, 2, 4), time = c(2014,2015,2016), year = 2015)
fp(cp = c(1, 2, 3), pp = c(NA, 2, 4), time = c(2014,2015,2016), year = 2015)
fp(cp = c(NA, 2, 3), pp = c(NA, NA, 4), time = c(2014,2015,2016), year = 2015)

}
