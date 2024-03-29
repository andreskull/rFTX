% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ftx.R
\name{ftx_future_funding_rates}
\alias{ftx_future_funding_rates}
\title{FTX Future Funding Rates}
\usage{
ftx_future_funding_rates(
  key,
  secret,
  ...,
  markets = c(),
  start_time = NA,
  end_time = NA,
  tz = "GMT"
)
}
\arguments{
\item{key}{A client's key (deprecated)}

\item{secret}{A client's secret (deprecated)}

\item{...}{Additional parameters to pass to API request}

\item{markets}{Vector of names of markets.}

\item{start_time}{POSIXct value from when to extract trades.}

\item{end_time}{POSIXct value up-to when to extract trades.}

\item{tz}{Timezone to display times in. Default is GMT.}
}
\value{
A list of three elements: a logical vector success: FALSE/TRUE,
failure_reason: reason for failure if success is FALSE, NA otherwise,
data: a tibble containing the data if success is TRUE
}
\description{
Returns the funding rates of futures
}
\examples{
ftx_future_funding_rates(key="", secret="", markets=c('CRV-PERP','XRP-PERP'))
}
