#' @importFrom magrittr %>%
#' @importFrom lubridate now
#' @importFrom httr GET POST DELETE add_headers content
#' @importFrom dplyr bind_rows mutate filter select
#' @importFrom logging logerror loginfo
#' @importFrom tibble tibble as_tibble add_column
#' @importFrom purrr set_names map_df
#' @importFrom rlang := 

utils::globalVariables(c(".", "total", "account", "future", "startTime", "high", "low", "volume", "market", "size", "base_url"))


#' @title FTX Initialise Endpoint Value
#' @description Changes the deafult endpoint value. Default endpoint is https://ftx.com
#' @param rftx_base_url Optional parameter. An endpoint value such as https://ftx.com or https://ftx.us
#' @export
ftx_init <- function(rftx_base_url = NA){
  if(!missing(rftx_base_url) | !is.na(rftx_base_url)){
    utils::assignInNamespace("base_url", rftx_base_url, ns="rFTX", envir=as.environment("package:rFTX"))
  } else {
    endpoints <- c("https://ftx.com", "https://ftx.us")
    selected_url <- utils::menu(endpoints, title = "\nWhich endpoint would you like to use?")
    print(paste("Using", endpoints[selected_url], "as the endpoint."))
    ftx_base_url <- endpoints[selected_url]
    utils::assignInNamespace("base_url", ftx_base_url, ns="rFTX", envir=as.environment("package:rFTX"))
  }
}

#' @title FTX Send Request
#' @description A helper function
#' @param method REST API Method such as GET or POST
#' @param path An additional path defined for each function
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param body Only for POST method. A named list of values containing market name (string), 
#' side ("buy" or "sell"), price (numeric), size (numeric), type ("limit" or "market"), 
#' reduceOnly (logical), ioc (logical), postOnly (logical) and clientId (numeric or NA)
#' @param ... Additional parameters to pass to API request
#' @return A response object as a list containing two elements, a logical vector success of 1 length and 
#' either an error element if success is FALSE or result list if success is TRUE.,
#' @noRd 

ftx_send_request <- function(method, path, key, secret, subaccount, body, ...) {
  url <- paste0(base_url, path)
  fn <- get(method)
  
  ts <- now("UTC") %>% as.integer() * 1000
  signature_payload <- paste0(ts, method, path)
  
  if(method == "POST" & !missing(body)){
    body_sign <- jsonlite::toJSON(body, auto_unbox = T)
    signature_payload <- paste0(signature_payload, body_sign)
  }
  
  signature <- digest::hmac(enc2utf8(secret),
                            enc2utf8(signature_payload),
                            algo = "sha256") 
  
  headers_vec <- c(`FTX-KEY` = key,
                   `FTX-SIGN` = signature,
                   `FTX-TS` = as.character(ts))
  
  if(!missing(subaccount) && !is.na(subaccount)){
    headers_vec <- c(headers_vec, `FTX-SUBACCOUNT` = subaccount)
  }
  
  r <- fn(url, add_headers(.headers = headers_vec), ...)
  
  if(method == "POST" & !missing(body)){
    r <- fn(url, add_headers(.headers = headers_vec), body = body, encode = "json", ...)  
  }
  
  response <- content(r, "parsed")
  if (response$success == FALSE) {
    logerror(msg = response$error, ...)
  }
  response
}

#' @title Result Formatter
#' @description A helper function
#' @param result The result of an API response
#' @param time_label time column to be formatted to POSIXct
#' @param tz Timezone to display times in. Default is GMT.
#' @return A formatted tibble
#' @noRd 

result_formatter <- function(result, time_label, tz){
  
  df <- bind_rows(result)
  df <- format_helper(df, time_label, tz)
  
  return(df)
}

#' @title Helper Function for the Formatter
#' @description A helper function
#' @param list A list or section of list from API response
#' @param time_label time column to be formatted to POSIXct
#' @param tz Timezone to display times in. Default is GMT.
#' @return A formatted tibble
#' @noRd

format_helper <- function(obj, time_label, tz){
  df <- obj %>%
    replace(lengths(.) == 0, NA) %>% 
    tibble::as_tibble() %>%
    mutate("{time_label}" := if(time_label %in% names(.))
      as.POSIXct(gsub("(.*):", "\\1", get(time_label)), format = "%Y-%m-%dT%H:%M:%OS%z", tz = tz) else NULL)
  return(df)
}

# Functions should return result content in the form of dataframe if result is available and success
# in case of response$success == FALSE the function should return the cause of failure if the FTX API provides it
# Test with the real failure cases against FTX and decide case-by-case

#' @title FTX Coin Balances
#' @description Returns balances coin balances of all accounts if accounts argument is left empty or returns the balances of the specified accounts.
#' @param key A client's key
#' @param secret A client's secret
#' @param accounts Optional parameter. A vector of client sub-accounts
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_coin_balances(key="", secret="", accounts = c())
#' @export

ftx_coin_balances <- function(key, secret, accounts = c(), ...) {
  response = ftx_send_request(method = "GET", path = '/api/wallet/all_balances', key, secret, subaccount = NA, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  # add response$success == FALSE handling
  result = response$result
  
  if(length(result) > 0){
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

#' @title FTX Positions
#' @description Returns the account positions
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_positions(key="", secret="", subaccount=NA)
#' @export

ftx_positions <- function(key, secret, subaccount = NA, tz = "GMT", ...) {
  # GET /positions
  response = ftx_send_request(method = "GET", path = '/api/positions', key, secret, subaccount, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  result = response$result
  
  if(length(result) > 0){
    df <- result_formatter(result, "time", tz) %>% 
      filter(size != 0)
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
#' @description Returns information on all types of markets on FTX
#' @param key A client's key
#' @param secret A client's secret
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_coin_markets(key="", secret="")
#' @export

ftx_coin_markets <- function(key, secret, tz = "GMT", ...) {
  # GET /markets
  response = ftx_send_request(method = "GET", path = '/api/markets', key, secret, subaccount = NA, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  result = response$result
  
  df <- result_formatter(result, "time", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Orderbook
#' @description Returns the orderbook for the market specified
#' @param key A client's key
#' @param secret A client's secret
#' @param market Name of market
#' @param depth Market depth. Min 1, Max 100, default 5
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_orderbook(key="", secret="", market = "1INCH/USD", depth = 5)
#' @export

ftx_orderbook <- function(key, secret, market = NA, depth = 5, ...) {
  # GET /markets/{market}/orderbook?depth={depth}
  # depth parameter check
  if(depth > 100) loginfo(msg = 'Depth value is too large. Using max value = 100.')
  if(depth < 1){ 
    logerror(msg = 'Depth value is too small. Min value is 1.')
    return(list(success = F, failure_reason = 'Depth value is too small. Min value is 1.', data = NULL))
  }
  
  path = paste0('/api/markets/', market, '/orderbook?depth=', depth)
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount = NA, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
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

#' @title FTX Trades
#' @description Returns the trades that have taken place for a particular market
#' @param key A client's key
#' @param secret A client's secret
#' @param market Name of market
#' @param start_time Optional parameter. POSIXct value from when to extract trades.
#' @param end_time Optional parameter. POSIXct value up-to when to extract trades.
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_trades(key="", secret="", market = "1INCH/USD")
#' @export

ftx_trades <- function(key, secret, market, start_time = NA, end_time = NA, tz = "GMT", ...) {
  # GET /markets/{market}/trades
  # add optional parameters
  path = paste0('/api/markets/', market, '/trades')
  
  if(!missing(start_time) & !is.na(start_time) & !missing(end_time) & !is.na(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  
  query_list <- list()
  
  if(!missing(start_time) & !is.na(start_time)){
    query_list['start_time'] <- as.integer(start_time)
  }
  if(!missing(end_time) & !is.na(end_time)){
    query_list['end_time'] <- as.integer(end_time)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount = NA, body = NULL, 
                              query = query_list, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "time", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Historical Prices
#' @description Returns historical prices of expired futures
#' @param key A client's key
#' @param secret A client's secret
#' @param market Name of market
#' @param resolution Window length in seconds. options: 15, 60, 300, 900, 3600, 14400, 86400, or any multiple of 86400 up to 30*86400
#' @param start_time Optional parameter. POSIXct value from when to extract trades.
#' @param end_time Optional parameter. POSIXct value up-to when to extract trades.
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_historical_prices(key="", secret="", market = "1INCH/USD")
#' @export

ftx_historical_prices <- function(key, secret, market, resolution = 14400, start_time = NA, end_time = NA, tz = "GMT", ...) {
  # GET /markets/{market}/candles?resolution={resolution}&start_time={start_time}&end_time={end_time}
  # check if resolution, start_time and end_time are correct and not contradictory, log error if not
  # check resolution parameter
  res_values <- c(0, 15, 60, 300, 900, 3600, 14400, 86400)
  if(resolution == 0 | !(resolution%%86400) %in% res_values | resolution > (30*86400)) {
    logerror(msg = 'Unsupported candle resolution value. Supported values are 15, 60, 300, 900, 
             3600, 14400, 86400, or any multiple of 86400 up to 30*86400')
    return(list(success = F, failure_reason = 'Unsupported candle resolution value. Supported values are 15, 60, 300, 900, 3600, 14400, 86400, or any multiple of 86400 up to 30*86400', data = NULL))
  }
  
  # check start and end times
  if(!missing(start_time) & !is.na(start_time) & !missing(end_time) & !is.na(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  
  path = paste0('/api/markets/', market, '/candles?resolution=', resolution)
  
  query_list <- list()
  
  if(!missing(start_time) & !is.na(start_time)){
    query_list['start_time'] <- as.integer(start_time)
  }
  if(!missing(end_time) & !is.na(end_time)){
    query_list['end_time'] <- as.integer(end_time)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount = NA, body = NULL,
                              query = query_list, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "startTime", tz)
  if (nrow(df)) {
    df <- df %>% select(start_time = startTime, open, high, low, close, volume)
  }
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Future Markets
#' @description Returns all types of futures on FTX
#' @param key A client's key
#' @param secret A client's secret
#' @param market Name of market
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_future_markets(key="", secret="")
#' @export

ftx_future_markets <- function(key, secret, market = NA, tz = "GMT", ...) {
  # GET /futures (if market == NA)
  # GET /futures/{market} (if market != NA)
  path = paste0('/api/futures')
  if(!is.na(market)){
    path = paste0(path, '/', market)
  }
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount = NA, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "expiry", tz)
  
  if(!is.na(market) & length(market) == 1){
    df <- format_helper(result, "expiry", tz)
  }
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Future Stats
#' @description Returns stats on futures 
#' @param key A client's key
#' @param secret A client's secret
#' @param market Name of market
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_future_stat(key="", secret="", market = "CRV-PERP") 
#' @export

ftx_future_stat <-  function(key, secret, market, tz = "GMT", ...) {
  # GET /futures/{market}/stats

  path = paste0('/api/futures/', market, '/stats')
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount = NA, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- format_helper(result, "nextFundingTime", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Future Funding Rates
#' @description Returns the funding rates of futures
#' @param key A client's key
#' @param secret A client's secret
#' @param markets Vector of names of markets. 
#' @param start_time POSIXct value from when to extract trades.
#' @param end_time POSIXct value up-to when to extract trades.
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_future_funding_rates(key="", secret="", markets=c('CRV-PERP','XRP-PERP'))
#' @export

ftx_future_funding_rates <-  function(key, secret, markets=c(), start_time=NA, end_time=NA, tz = "GMT", ...) {
  # GET /funding_rates
  # checks on start and end times
  if(!missing(start_time) & !is.na(start_time) & !missing(end_time) & !is.na(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  path = '/api/funding_rates'
  
  query_list <- list()
  
  if(!missing(start_time) & !is.na(start_time)){
    query_list['start_time'] <- as.integer(start_time)
  }
  if(!missing(end_time) & !is.na(end_time)){
    query_list['end_time'] <- as.integer(end_time)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount = NA, body = NULL,
                              query = query_list, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "time", tz)
  
  if(length(result) > 0 & !missing(markets)){
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
#' @description Returns information on the open orders for account or subaccount if specified
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param markets Vector of names of markets. 
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_open_orders(key="", secret="", subaccount=NA, markets=c('CRV-PERP','XRP-PERP'))
#' @export

ftx_open_orders <- function(key, secret, subaccount, markets=c(), tz = "GMT", ...) {
  # GET /orders?market={market}
  path = paste0('/api/orders')
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "createdAt", tz)
  
  if(length(result) > 0 & !missing(markets)){
    df <- df %>%
      filter(market %in% markets)
  }
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Orders History
#' @description Returns the history of orders for the account or subaccount if specified
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param markets Vector of names of markets. 
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_orders_history(key="", secret="", subaccount=NA, markets=c('CRV-PERP','XRP-PERP'))
#' @export

ftx_orders_history <- function(key, secret, subaccount, markets=c(), tz = "GMT", ...) {
  # GET /orders/history?market={market}
  path = paste0('/api/orders/history')

  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  result = response$result
  
  df <- result_formatter(result, "createdAt", tz)
  
  if(length(result) > 0 & !missing(markets)){
    df <- df %>%
      filter(market %in% markets)
  }
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Place Order
#' @description Places an order based on the information provided
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
#' @param client_id optional; client order id
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_place_order(key="",secret="",subaccount=NA,market="XRP-PERP",
#' side="buy",price=1,type="limit",size=3)
#' @export

ftx_place_order <-  function(key, secret, subaccount, market=NA, side=NA, price=NA, type=NA, size=NA, reduceOnly=FALSE, ioc=FALSE, postOnly=FALSE, client_id=NA, tz = "GMT", ...) {

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
  body$clientId = client_id
  response = ftx_send_request(method = "POST", path = path, key, secret, subaccount, body = body, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- format_helper(result, "createdAt", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Modify Order
#' @description Modifies an order's size and price
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param order_id Numeric value of order ID
#' @param size Size of order
#' @param price Price of order 
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_modify_order(key="", secret="", subaccount=NA, order_id=1234, size=2, price=1)
#' @export

ftx_modify_order <- function(key, secret, subaccount, order_id, size, price, tz = "GMT", ...) {
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
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- format_helper(result, "createdAt", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Order Status
#' @description Returns the status of orders
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param order_id Numeric value of order ID
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_order_status(key="", secret="", subaccount=NA, order_id=1234)
#' @export

ftx_order_status <- function(key, secret, subaccount, order_id, tz = "GMT", ...) {
  # GET /orders/by_client_id/{client_order_id}
  path = paste0('/api/orders/', order_id)
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  n_results <- length(result$id)
  
  result <- result %>% lapply(function(x) {
    if (class(x) == "NULL") rep(NA, n_results)
    else x
  })
  
  df <- format_helper(result, "createdAt", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Cancel Order
#' @description Queues an order for cancellation
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param order_id Numeric value of order ID
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' result: if success is TRUE: "Order queued for cancellation"
#' @examples ftx_cancel_order(key="", secret="", subaccount="", order_id=1234)
#' @export

ftx_cancel_order <- function(key, secret, subaccount, order_id, ...) {
  # DELETE /orders/{order_id}
  path = paste0('/api/orders/', order_id)
  response = ftx_send_request(method = "DELETE", path = path, key, secret, subaccount, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    result = result
  )
  return(return_obj)
}

#' @title FTX Modify Order by Client ID
#' @description Modifies an order using the client ID instead of the order ID
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param client_id Character string of client order ID
#' @param new_client_id Character string of new client order ID
#' @param size Size of order
#' @param price Price of order 
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_modify_order_clientid(key="", secret="", subaccount=NA, client_id="order1", 
#' new_client_id="order2", size=2, price=1)
#' @export

ftx_modify_order_clientid <- function(key, secret, subaccount, client_id, new_client_id, size, price, tz = "GMT", ...) {

  # POST /orders/by_client_id/{client_order_id}/modify
  path = paste0('/api/orders/by_client_id/', client_id, '/modify')
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
  
  if(!missing(client_id)){
    body$clientId = new_client_id
  }
  
  response = ftx_send_request(method = "POST", path = path, key, secret, subaccount, body = body, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- format_helper(result, "createdAt", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Order Status by Client ID
#' @description Returns the status of orders using the client ID instead of the order ID
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param client_id Character string of client order ID
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_order_status_clientid(key="", secret="", subaccount=NA, client_id="order1")
#' @export

ftx_order_status_clientid <- function(key, secret, subaccount, client_id, tz = "GMT", ...) {
  # GET /orders/by_client_id/{client_order_id}
  path = paste0('/api/orders/by_client_id/', client_id)
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  n_results <- length(result$id)
  
  result <- result %>% lapply(function(x) {
    if (class(x) == "NULL") rep(NA, n_results)
    else x
  })
  
  df <- format_helper(result, "createdAt", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Cancel Order by Client ID
#' @description Queues an order for cancellation using the client ID instead of the order ID
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param client_id Character string of client order ID
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' result: if success is TRUE: "Order queued for cancellation"
#' @examples ftx_cancel_order_clientid(key="", secret="", subaccount=NA, client_id="order1")
#' @export

ftx_cancel_order_clientid <- function(key, secret, subaccount, client_id, ...) {
  # DELETE /orders/by_client_id/{client_order_id}
  path = paste0('/api/orders/by_client_id/', client_id)
  response = ftx_send_request(method = "DELETE", path = path, key, secret, subaccount, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    result = result
  )
  return(return_obj)
}

#' @title FTX Orders Fills
#' @description Returns market fills
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param markets Vector of names of markets. 
#' @param start_time Optional parameter. POSIXct value from when to extract trades.
#' @param end_time Optional parameter. POSIXct value up-to when to extract trades.
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_order_fills(key="", secret="", subaccount=NA, markets=c('CRV-PERP','XRP-PERP'))
#' @export

ftx_order_fills <- function(key, secret, subaccount, markets=c(), start_time=NA, end_time=NA, tz = "GMT", ...) {
  # GET /fills?market={market} 
  path = '/api/fills'
  
  if(!missing(start_time) & !is.na(start_time) & !missing(end_time) & !is.na(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  query_list <- list()
  
  if(!missing(start_time) & !is.na(start_time)){
    query_list['start_time'] <- as.integer(start_time)
  }
  if(!missing(end_time) & !is.na(end_time)){
    query_list['end_time'] <- as.integer(end_time)
  }
  
  response = ftx_send_request(method = "GET", path = path, key, secret, subaccount, body = NULL, 
                              query = query_list, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "time", tz)
  
  if(length(result) > 0 & !missing(markets)){
    df <- df %>%
      filter(market %in% markets)
  }
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Funding Payments
#' @description Returns the funding payments for futures
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param start_time Optional parameter. POSIXct value from when to extract trades.
#' @param end_time Optional parameter. POSIXct value up-to when to extract trades.
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_funding_payments(key="", secret="", subaccount=NA)
#' @export

ftx_funding_payments <-  function(key, secret, subaccount, start_time = NA, end_time = NA, tz = "GMT", ...) {
  # GET /funding_payments
  if(!missing(start_time) & !is.na(start_time) & !missing(end_time) & !is.na(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  query_list <- list()
  
  if(!missing(start_time) & !is.na(start_time)){
    query_list['start_time'] <- as.integer(start_time)
  }
  if(!missing(end_time) & !is.na(end_time)){
    query_list['end_time'] <- as.integer(end_time)
  }
  response = ftx_send_request(method = "GET", path = '/api/funding_payments', key, secret, 
                              subaccount, body = NULL, query = query_list, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "time", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Spot Lending History
#' @description Returns the lending history for coins
#' @param key A client's key
#' @param secret A client's secret
#' @param start_time Optional parameter. POSIXct value from when to extract trades.
#' @param end_time Optional parameter. POSIXct value up-to when to extract trades.
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_spot_lending_history(key="", secret="")
#' @export

ftx_spot_lending_history <- function(key, secret, start_time=NA, end_time=NA, tz = "GMT", ...) {
  # GET /spot_margin/history
  if(!missing(start_time) & !is.na(start_time) & !missing(end_time) & !is.na(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  query_list <- list()
  
  if(!missing(start_time) & !is.na(start_time)){
    query_list['start_time'] <- as.integer(start_time)
  }
  if(!missing(end_time) & !is.na(end_time)){
    query_list['end_time'] <- as.integer(end_time)
  }
  
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/history', key, secret, subaccount = NA, body = NULL,
                              query = query_list, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "time", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Spot Margin Borrow Rates
#' @description Returns the estimated hourly borrow rate for the next spot margin cycle and the hourly borrow rate in the previous spot margin cycle for coins.
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_spot_margin_borrow_rates(key="", secret="", subaccount=NA)
#' @export

ftx_spot_margin_borrow_rates <- function(key, secret, subaccount, tz = "GMT", ...) {
  # GET /spot_margin/borrow_rates
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/borrow_rates', key, secret, subaccount, body = NULL, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "time", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

#' @title FTX Spot Borrow History
#' @description Returns the coin borrow history for the user
#' @param key A client's key
#' @param secret A client's secret
#' @param subaccount A client's subaccount
#' @param start_time Optional parameter. POSIXct value from when to extract trades.
#' @param end_time Optional parameter. POSIXct value up-to when to extract trades.
#' @param tz Timezone to display times in. Default is GMT.
#' @param ... Additional parameters to pass to API request
#' @return A list of three elements: a logical vector success: FALSE/TRUE, 
#' failure_reason: reason for failure if success is FALSE, NA otherwise, 
#' data: a tibble containing the data if success is TRUE
#' @examples ftx_my_spot_borrow_history(key="", secret="", subaccount=NA)
#' @export

ftx_my_spot_borrow_history <- function(key, secret, subaccount, start_time=NA, end_time=NA, tz = "GMT", ...) {
  # GET /spot_margin/borrow_history
  if(!missing(start_time) & !is.na(start_time) & !missing(end_time) & !is.na(end_time)){
    if(start_time > end_time){
      logerror(msg = 'Start date cannot be after end date.')
    }
  }
  query_list <- list()
  
  if(!missing(start_time) & !is.na(start_time)){
    query_list['start_time'] <- as.integer(start_time)
  }
  if(!missing(end_time) & !is.na(end_time)){
    query_list['end_time'] <- as.integer(end_time)
  }
  response = ftx_send_request(method = "GET", path = '/api/spot_margin/borrow_history', key, secret, 
                              subaccount, body = NULL, query = query_list, ...)
  if(!response$success) return(list(success = F, failure_reason = response$error, data = NULL))
  
  result = response$result
  
  df <- result_formatter(result, "time", tz)
  
  return_obj <- list(
    success = response$success,
    failure_reason = ifelse(response$success, NA, response$error),
    data = df
  )
  return(return_obj)
}

