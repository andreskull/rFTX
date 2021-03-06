% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ftx.R
\name{ftx_trades}
\alias{ftx_trades}
\title{FTX Trades}
\usage{
ftx_trades(
  key,
  secret,
  market,
  ...,
  start_time = NA,
  end_time = NA,
  tz = "GMT"
)
}
\arguments{
\item{key}{A client's key}

\item{secret}{A client's secret}

\item{market}{Name of market}

\item{...}{Additional parameters to pass to API request}

\item{start_time}{Optional parameter. POSIXct value from when to extract trades.}

\item{end_time}{Optional parameter. POSIXct value up-to when to extract trades.}

\item{tz}{Timezone to display times in. Default is GMT.}
}
\value{
A list of three elements: a logical vector success: FALSE/TRUE,
failure_reason: reason for failure if success is FALSE, NA otherwise,
data: a tibble containing the data if success is TRUE
}
\description{
Returns the trades that have taken place for a particular market
}
\examples{
ftx_trades(key="", secret="", market = "1INCH/USD")
}
