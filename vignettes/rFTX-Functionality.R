## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  # Loading the library
#  library(rFTX)
#  
#  # Example API key and secret
#  key <- "LR0RQT6bKjrUNh38eCw9jYC89VDAbRkCogAc_XAm"
#  secret <- "T4lPid48QtjNxjLUFOcUZghD7CUJ7sTVsfuvQZF2"
#  
#  obj <- ftx_coin_balances(key, secret, accounts = c())
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_positions(key, secret, subaccount=NA)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_coin_markets(key, secret)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_orderbook(key, secret, market = NA, depth = 10)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_trades(key, secret, market, start_time = NA, end_time = NA)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_historical_prices(key, secret, market, resolution = 14400, start_time = NA, end_time = NA)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_future_markets(key, secret, market = NA)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_future_stat(key, secret, market)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_future_funding_rates(key, secret, markets=c(), start_time, end_time)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_open_orders(key, secret, subaccount, markets=c())
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_orders_history(key, secret, subaccount, markets=c())
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_place_order(key, secret, subaccount, market=NA, side=NA, price=NA, type=NA, size=NA,
#                         reduceOnly=FALSE, ioc=FALSE, postOnly=FALSE, clientId=NA)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_modify_order(key, secret, subaccount, order_id, size, price)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_modify_order_clientid(key, secret, subaccount, client_id, size, price)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_order_status(key, secret, subaccount, order_id)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_order_status_clientid(key, secret, subaccount, client_id)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_cancel_order(key, secret, subaccount, order_id)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_cancel_order_clientid(key, secret, subaccount, client_id)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_order_fills(key, secret, subaccount, markets=c(), start_time=NA, end_time=NA)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_funding_payments(key, secret, subaccount, start_time = NA, end_time = NA)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_spot_lending_history(key, secret, start_time=NA, end_time=NA)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_spot_margin_borrow_rates(key, secret, subaccount)
#  

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  obj <- ftx_my_spot_borrow_history(key, secret, subaccount, start_time, end_time)
#  

