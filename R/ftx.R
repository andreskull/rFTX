#' @exportPattern ^[[:alpha:]]+
#'

library(tidyverse)
library(digest)
library(lubridate)
library(stringi)
library(assert)
library(logging)
library(httr)

base_url <- "https://ftx.com"

#' @title FTX Send Request
#' @param method REST API Method such as GET or POST
#' @param path An additional path defined for each function
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @return A response returned by request using specified method.

ftx_send_request <- function(method, path, key, secret, subaccount, ...) {
  url <- paste0(base_url, path)
  fn <- get(method)
  
  ts <- now("UTC") %>% as.integer() * 1000
  signature_payload <- paste0(ts, method, path)
  
  signature <- digest::hmac(enc2utf8(secret),
                            enc2utf8(signature_payload),
                            algo = "sha256") 
  headers_vec <- c(`FTX-KEY` = key,
                   `FTX-SIGN` = signature,
                   `FTX-TS` = as.character(ts))
  if(!missing(subaccount)){
    headers_vec <- c(headers_vec, `FTX-SUBACCOUNT` = subaccount)
  }
  r <- fn(url, add_headers(.headers = headers_vec), ...)
  
  response <- content(r, "parsed")
  if (response$success == FALSE) {
    logerror(msg = response$error, ...)
  }
  response
}


# Functions should return result content in the form of dataframe if result is available and success
# in case of response$success == FALSE the function should return the cause of failure if the FTX API provides it
# Test with the real failure cases against FTX and decide case-by-case

#' @title FTX Coin Balances
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param accounts Optional parameter. A vector of client sub-accounts
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_coin_balances <- function(key, secret, subaccount, accounts = c(), ...) {
  response = ftx_send_request(method = "GET", path = '/api/wallet/all_balances', key, secret, subaccount, ...)
  
  # add response$success == FALSE handling
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result, n = names(result)), 1, function(x) {
    df <- map_df(x[[1]], tibble::as_tibble)
    df <- df %>% add_column(account = x[[2]]) %>%
      filter(total != 0)
  }
  ))
  if (length(accounts) > 0) {
    df <- df %>% filter(account %in% accounts)
  } else {
    df
  }
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Positions
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_positions <- function(key, secret, subaccount, ...) {
  # GET /positions
  response = ftx_send_request(method = "GET", path = '/api/positions', key, secret, subaccount, ...)
  result = response$result
  
  if(length(result) > 0){
    df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
      df <- x[[1]] %>%
        replace(lengths(.) == 0, NA)  %>% 
        tibble::as_tibble() %>%
        filter(size != 0)
    }
    ))
  } else {
    df <- tibble()
  }
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Coin Markets
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_coin_markets <- function(key, secret, subaccount, ...) {
  # GET /markets
  response = ftx_send_request(method = "GET", path = '/api/markets', key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Orderbook
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @param depth Market depth. Max 100, default 5
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_orderbook <- function(key, secret, subaccount, market, depth = 5, ...) {
  # GET /markets/{market}/orderbook?depth={depth}
  # depth parameter check
  if(depth > 100) loginfo(msg = 'Depth value is too large.Max value is 100.')
  
  path = paste0('/api/markets/', market, '/orderbook?depth=', depth)
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result, n = names(result)), 1, function(x) {
    df <- map_df(x[[1]], tibble::as_tibble, 
                 .name_repair = ~ vctrs::vec_as_names(...,repair="universal",quiet = T)) %>% 
      set_names(c('price','size')) %>% 
      add_column(name = x[[2]])
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Orderbook
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @param start_time Optional parameter. POSIXct value from when to extract trades.
#' @param end_time Optional parameter. POSIXct value up-to when to extract trades.
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_trades <- function(key, secret, subaccount, market, start_time, end_time, ...) {
  # GET /markets/{market}/trades
  # add optional parameters
  path = paste0('/api/markets/', market, '/trades')
  
  query_list <- list()
  
  if(!missing(start_time)){
    query_list['start_time'] <- as.numeric(start_time)
  }
  if(!missing(end_time)){
    query_list['end_time'] <- as.numeric(end_time)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, 
                              query = query_list, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Historical Prices
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @param resolution Window length in seconds. options: 15, 60, 300, 900, 3600, 14400, 86400, or any multiple of 86400 up to 30*86400
#' @param start_time Optional parameter. POSIXct value from when to extract trades.
#' @param end_time Optional parameter. POSIXct value up-to when to extract trades.
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_historical_prices <- function(key, secret, subaccount, market, resolution, start_time, end_time, ...) {
  # GET /markets/{market}/candles?resolution={resolution}&start_time={start_time}&end_time={end_time}
  # check if resolution, start_time and end_time are correct and not contradictory, log error if not
  # check resolution parameter
  res_values <- c(0, 15, 60, 300, 900, 3600, 14400, 86400)
  if(resolution == 0 | !(resolution%%86400) %in% res_values | resolution > (30*86400)) {
    logerror(msg = 'Unsupported candle resolution value. Supported values are 15, 60, 300, 900, 
             3600, 14400, 86400, or any multiple of 86400 up to 30*86400')
  }
  
  # check start and end times
  if(!missing(start_time) & !missing(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  
  path = paste0('/api/markets/', market, '/candles?resolution=', resolution)
  
  query_list <- list()
  
  if(!missing(start_time)){
    query_list['start_time'] <- as.numeric(start_time)
  }
  if(!missing(end_time)){
    query_list['end_time'] <- as.numeric(end_time)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, 
                              query = query_list, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Future Markets
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_future_markets <- function(key, secret, subaccount, market = NA, ...) {
  # GET /futures (if market == NA)
  # GET /futures/{market} (if market != NA)
  path = paste0('/api/futures')
  if(!is.na(market)){
    path = paste0(path, '/', market)
  }
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  
  if(!is.na(market) & length(market) == 1){
    df <- result %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Future Stats
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_future_stat <-  function(key, secret, subaccount, market, ...) {
  # GET /futures/{market}/stats
  if(length(market) == 0){
    logerror(msg = "Market name required.")
  }
  
  path = paste0('/api/futures/', market, '/stats')
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, ...)
  result = response$result
  
  df <- result %>%
    tibble::as_tibble()
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Future Funding Rates
#' @param key A client's key
#' @param secret A client's secret
#' @param markets Vector of names of markets. 
#' @param subaccount A client's subaccount
#' @param start_time POSIXct value from when to extract trades.
#' @param end_time POSIXct value up-to when to extract trades.
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_future_funding_rates <-  function(key, secret, subaccount, markets=c(), start_time, end_time, ...) {
  # GET /funding_rates
  # checks on start and end times
  if(!missing(start_time) & !missing(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  path = '/api/funding_rates'
  
  query_list <- list()
  
  if(!missing(start_time)){
    query_list['start_time'] <- as.numeric(start_time)
  }
  if(!missing(end_time)){
    query_list['end_time'] <- as.numeric(end_time)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, 
                              query = query_list, ...)
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  if(!missing(markets)){
    df <- df %>%
      filter(future %in% markets)
  }
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Open Orders
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_open_orders <- function(key, secret, subaccount, market, ...) {
  # GET /orders?market={market}
  path = paste0('/api/orders')
  if(!missing(market)){
    path = paste0(path, '?market=', market)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Orders History
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_orders_history <- function(key, secret, subaccount, market, ...) {
  # GET /orders/history?market={market}
  path = paste0('/api/orders/history')
  if(!missing(market)){
    path = paste0(path, '?market=', market)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Place Order
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @param side "buy" or "sell"
#' @param price Numeric value. Send null for market orders.
#' @param type "limit" or "market"
#' @param size Size of order
#' @param reduceOnly optional; default is false
#' @param ioc optional; default is false
#' @param postOnly optional; default is false
#' @param clientId optional; client order id
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_place_order <-  function(key, secret, subaccount, market=NA, side=NA, price=NA, type=NA, size=NA, reduceOnly=FALSE, ioc=FALSE, postOnly=FALSE, clientId=NA, ...) {
  # POST /orders
  # check if side, price, type, size, reduce_only, ioc, postonly parameters are correct
  path = paste0('/api/orders')
  body = list()
  if(!missing(market)){
    body$market = market
  }
  if(!missing(side)){
    if(side %in% c('buy','sell')){
      body$side = side
    }
  }
  if(!missing(price)){
    if(is.numeric(price) | is.null(price)){
      body$price = price
    }
  }
  if(!missing(type)){
    if(type %in% c('limit','market')){
      body$type = type
    }
  }
  if(is.numeric(size)) body$size = size
  if(reduceOnly %in% c(T,F)) body$reduceOnly = reduceOnly
  if(ioc %in% c(T,F)) body$ioc = ioc
  if(postOnly %in% c(T,F)) body$postOnly = postOnly
  body$clientId = clientId
  response = ftx_send_request(method = "POST", path = path, key, secret, subaccount, body = body, ...)
  result = response$result
  
  df <- result %>%
    tibble::as_tibble()
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Modify Order
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param order_id Numeric value of order ID
#' @param size Size of order
#' @param price Price of order 
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_modify_order <- function(key, secret, subaccount, order_id, size, price, ...) {
  # POST /orders/{order_id}/modify
  path = paste0('/api/orders/', order_id, '/modify')
  body <- list()
  if(!missing(size)){
    if(is.numeric(size) | is.null(size)){
      body$size = size
    }
  }
  if(!missing(price)){
    if(is.numeric(price) | is.null(price)){
      body$price = price
    }
  }
  
  response = ftx_send_request(method = "POST", path = path, key, secret, subaccount, body = body, ...)
  result = response$result
  
  df <- result %>%
    tibble::as_tibble()
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Order Status
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param order_id Numeric value of order ID
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_order_status <- function(key, secret, subaccount, order_id, ...) {
  # GET /orders/by_client_id/{client_order_id}
  path = paste0('/api/orders/by_client_id/', order_id)
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, ...)
  result = response$result
  
  df <- result %>%
    tibble::as_tibble()
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Cancel Order
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param order_id Numeric value of order ID
#' @return A list of three elements: success: false/true, failure_reason: if available, result is successful: "Order queued for cancellation"

ftx_cancel_order <- function(key, secret, subaccount, order_id, ...) {
  # DELETE /orders/{order_id}
  path = paste0('/api/orders/', order_id)
  response = ftx_send_request(method = "DELETE", path = path, key, secret, subaccount, ...)
  result = response$result
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    result = result
  )
  return(return_obj)
}

#' @title FTX Orders Fills
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param market Name of market
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_order_fills <- function(key, secret, subaccount, market, ...) {
  # GET /fills?market={market} 
  path = '/api/fills'
  if(!missing(market)){
    path = paste0(path, '?market=', market)
  }
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Funding Payments
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_funding_payments <-  function(key, secret, subaccount, ...) {
  # GET /funding_payments
  response = ftx_send_request(method = "GET", path = '/api/funding_payments', key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Spot Lending History
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_spot_lending_history <- function(key, secret, subaccount, ...) {
  # GET /spot_margin/history
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/history', key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Spot Margin Borrow Rates
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_spot_margin_borrow_rates <- function(key, secret, subaccount, ...) {
  # GET /spot_margin/borrow_rates
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/borrow_rates', key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Spot Borrow History
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @return A list of three elements: success: false/true, failure_reason: if available, data: tibble

ftx_my_spot_borrow_history <- function(key, secret, subaccount, ...) {
  # GET /spot_margin/borrow_history
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/borrow_history', key, secret, subaccount, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      replace(lengths(.) == 0, NA) %>% 
      tibble::as_tibble()
  }
  ))
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

