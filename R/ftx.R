library(tidyverse)
library(digest)
library(lubridate)
library(stringi)
library(assert)
library(logging)
library(httr)

base_url <- "https://ftx.com"


ftx_send_request <- function(method, path, key, secret, ...) {
  url <- paste0(base_url, path)
  fn <- get(method)
  
  ts <- now("UTC") %>% as.integer() * 1000
  signature_payload <- paste0(ts, method, path)

  signature <- digest::hmac(enc2utf8(secret),
                            enc2utf8(signature_payload),
                            algo = "sha256") 
  r <- fn(url, add_headers(`FTX-KEY` = key,
                           `FTX-SIGN` = signature,
                           `FTX-TS` = as.character(ts)), ...)
  response <- content(r, "parsed")
  if (response$success == FALSE) {
    logerror(msg = response$error, ..., )
  }
  response
}

# Functions should return result content in the form of dataframe if result is available and success
# in case of response$success == FALSE the function should return the cause of failure if the FTX API provides it
# Test with the real failure cases against FTX and decide case-by-case

ftx_coin_balances <- function(key, secret, accounts = c(), ...) {
  response = ftx_send_request(method = "GET", path = '/api/wallet/all_balances', key, secret, ...)
  
  # add response$success == FALSE handling
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result, n = names(result)), 1, function(x) {
    df <- map_df(x[[1]], tibble::as_tibble)
    df <- df %>% add_column(account = x[[2]])
  }
  ))
  if (length(accounts) > 0) {
    df <- df %>% filter(account %in% accounts)
  } else {
    df
  }
}


ftx_positions <- function(key, secret, ...) {
  # GET /positions
  response = ftx_send_request(method = "GET", path = '/api/positions', key, secret, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}


ftx_coin_markets <- function(key, secret, ...) {
  # GET /markets
  response = ftx_send_request(method = "GET", path = '/api/markets', key, secret, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_orderbook <- function(key, secret, market, depth = 5, ...) {
  # GET /markets/{market}/orderbook?depth={depth}
  # depth parameter check
  if(depth > 100) loginfo(msg = 'Depth value is too large.Max value is 100.')
  
  path = paste0('/api/markets/', market, '/orderbook?', depth)
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result, n = names(result)), 1, function(x) {
    df <- map_df(x[[1]], tibble::as_tibble, 
                 .name_repair = ~ vctrs::vec_as_names(...,repair="universal",quiet = T)) %>% 
      set_names(c('price','size')) %>% 
      add_column(name = x[[2]])
  }
  ))
}

ftx_trades <- function(key, secret, market, start_time, end_time, ...) {
  # GET /markets/{market}/trades
  # add optional parameters
  path = paste0('/api/markets/', market, '/trades')
  
  if(!missing(start_time)){
    path = paste0(path, '?start_time=', start_time)
  }
  if(!missing(end_time)){
    if(missing(start_time)){
      path = paste0(path, '?end_time=', end_time)
    } else {
      path = paste0(path, '&?end_time=', end_time)
    }
  }
    
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_historical_prices <- function(key, secret, market, resolution, start_time, end_time, ...) {
  # GET /markets/{market}/candles?resolution={resolution}&start_time={start_time}&end_time={end_time}
  # check if resolution, start_time and end_time are correct and not contradictory, log error if not
  # check resolution parameter
  res_values <- c(0, 15, 60, 300, 900, 3600, 14400, 86400)
  if(resolution == 0 | !(resolution%%86400) %in% res_values) {
    logerror(msg = 'Unacceptable resolution value. Acceptable values are 15, 60, 300, 900, 3600, 14400, 86400, or any multiple of 86400 up to 30*86400')
  }
  
  # check start and end times
  if(!missing(start_time) & !missing(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  
  path = paste0('/api/markets/', market, '/candles?resolution=', resolution)
  
  if(!missing(start_time)){
    path = paste0(path, '&start_time=', start_time)
  }
  if(!missing(end_time)){
    path = paste0(path, '&end_time=', end_time)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_future_markets <- function(key, secret, market = NA, ...) {
  # GET /futures (if market == NA)
  # GET /futures/{market} (if market != NA)
  path = paste0('/api/futures')
  if(!is.na(market)){
    path = paste0(path, '/', market)
  }
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  result = response$result
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
  
  if(!is.na(market) & length(market) == 1){
    df <- result %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
}

ftx_future_stat <-  function(key, secret, market, ...) {
  # GET /futures/{market}/stats
  if(length(market) == 0){
    logerror(msg = "Market name required.")
  }
  
  path = paste0('/api/futures/', market, '/stats')
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  result = response$result
  
  df <- result %>%
    tibble::as_tibble()
}

ftx_future_funding_rates <-  function(key, secret, market, start_time, end_time, ...) {
  # GET /funding_rates
  # checks on start and end times
  if(!missing(start_time) & !missing(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  
  path = '/api/funding_rates'
  if(!missing(market)){
    path = paste0(path, '?future=', market)
  }
  if(!missing(start_time)){
    if(missing(market)){
      path = paste0(path, '?start_time=', start_time)
    } else {
      path = paste0(path, '&?start_time=', start_time)
    }
  }
  if(!missing(end_time)){
    if(missing(start_time)){
      path = paste0(path, '?end_time=', end_time)
    } else {
      path = paste0(path, '&?end_time=', end_time)
    }
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_open_orders <- function(key, secret, market, ...) {
  # GET /orders?market={market}
  path = paste0('/api/orders')
  if(!missing(market)){
    path = paste0(path, '?market=', market)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_orders_history <- function(key, secret, market, ...) {
  # GET /orders/history?market={market}
  path = paste0('/api/orders/history')
  if(!missing(market)){
    path = paste0(path, '?market=', market)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  result = response$result
  
  df <- do.call(rbind, apply(tibble(r = result), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_place_order <-  function(key, secret, market=NA, side=NA, price=NA, type=NA, size=NA, reduce_only=FALSE, ioc=FALSE, postOnly=FALSE, client_id=NA, ...) {
  # POST /orders
  # check if side, price, type, size, reduce_only, ioc, postonly parameters are correct
  
}

ftx_modify_order <- function(key, secret, order_id, size, price, ...) {
  # POST /orders/{order_id}/modify
}

ftx_order_status <- function(key, secret, order_id, ...) {
  # GET /orders/by_client_id/{client_order_id}
  path = paste0('/api/orders/by_client_id/', order_id)
  response = ftx_send_request(method = "GET", path = path, key, secret, ...)
  
  df <- response %>%
    tibble::as_tibble()
}

ftx_cancel_order <- function(key, secret, order_id, ...) {
  # DELETE /orders/{order_id}
}

ftx_order_fills <- function(key, secret, market, ...) {
  # GET /fills?market={market} 
  
}

ftx_funding_payments <-  function(key, secret, ...) {
  # GET /funding_payments
  response = ftx_send_request(method = "GET", path = '/api/funding_payments', key, secret, ...)
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = response), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_spot_lending_history <- function(key, secret, ...) {
  # GET /spot_margin/history
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/history', key, secret, ...)
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = response), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_spot_margin_borrow_rates <- function(key, secret, ...) {
  # GET /spot_margin/borrow_rates
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/borrow_rates', key, secret, ...)
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = response), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

ftx_my_spot_borrow_history <- function(key, secret, ...) {
  # GET /spot_margin/borrow_history
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/borrow_history', key, secret, ...)
  
  df <- do.call(plyr::rbind.fill, apply(tibble(r = response), 1, function(x) {
    df <- x[[1]] %>%
      purrr::modify_if(is.null, list) %>% 
      tibble::as_tibble()
  }
  ))
}

