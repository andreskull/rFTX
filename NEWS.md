# rFTX 0.1.1

## New functions
* `ftx_get_multiple_markets()`
* `ftx_get_hourly_markets()`

## Deprecated functions
* `ftx_coin_markets()` (the function will be removed in v.0.2.0)

## Deprecated parameters
* `key` and `secret` parameters are deprecated in functions that do not require authentication (the parameters will be removed in v.0.2.0):
  * `ftx_orderbook()`
  * `ftx_trades()`
  * `ftx_historical_prices()`
  * `ftx_future_markets()`
  * `ftx_future_stat()`
  * `ftx_future_funding_rates()`
  
