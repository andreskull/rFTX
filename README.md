
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rFTX

<!-- badges: start -->
<!-- badges: end -->

The goal of rFTX is to provide a way of making GET, POST and DELETE
requests in R via the FTX REST API. The user is required to have an FTX
account as the account’s API key and secret key is used in
authentication. These can be created here: <https://ftx.com/profile>. If
you are using a subaccount, some functions allow the name as an
argument. This can be left out if not using subaccounts.

## Installation

rFTX package is not yet on CRAN but it will soon be.

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("andreskull/rFTX")
```

## GET Example

This is a basic example which shows you how to fetch trades data for a
particular market:

``` r
library(rFTX)

## basic example code
ftx_trades(key = "", secret = "", market = "FTM-PERP")
#> $success
#> [1] TRUE
#> 
#> $failure_reason
#> [1] NA
#> 
#> $data
#>            id   price size side liquidation                time
#> 1  2326804932 2.62400  218 sell        TRUE 2021-11-03 12:40:25
#> 2  2326804651 2.62490  227 sell        TRUE 2021-11-03 12:40:24
#> 3  2326804650 2.62520   16 sell        TRUE 2021-11-03 12:40:24
#> 4  2326804059 2.62555  642  buy       FALSE 2021-11-03 12:40:22
#> 5  2326804058 2.62555  625  buy       FALSE 2021-11-03 12:40:22
#> 6  2326804057 2.62545   17  buy       FALSE 2021-11-03 12:40:22
#> 7  2326803871 2.62545   13  buy       FALSE 2021-11-03 12:40:21
#> 8  2326803533 2.62545   50  buy       FALSE 2021-11-03 12:40:20
#> 9  2326803338 2.62525   14 sell       FALSE 2021-11-03 12:40:19
#> 10 2326803337 2.62525  180 sell       FALSE 2021-11-03 12:40:19
#> 11 2326802750 2.62545  394  buy       FALSE 2021-11-03 12:40:17
#> 12 2326802708 2.62425  470  buy       FALSE 2021-11-03 12:40:17
#> 13 2326802641 2.62525  385  buy       FALSE 2021-11-03 12:40:17
#> 14 2326802640 2.62525 1100  buy       FALSE 2021-11-03 12:40:17
#> 15 2326802639 2.62525   25  buy       FALSE 2021-11-03 12:40:17
#> 16 2326802638 2.62525   55  buy       FALSE 2021-11-03 12:40:17
#> 17 2326802637 2.62515  191  buy       FALSE 2021-11-03 12:40:17
#> 18 2326802491 2.62350  205  buy       FALSE 2021-11-03 12:40:16
#> 19 2326802490 2.62340  470  buy       FALSE 2021-11-03 12:40:16
#> 20 2326802248 2.62205 2231 sell       FALSE 2021-11-03 12:40:15
```

This is another example which shows you how to fetch future funding
rates data for two markets:

``` r
## second example code
ftx_future_funding_rates(key = "", secret = "", markets = c('CRV-PERP','XRP-PERP'))
#> $success
#> [1] TRUE
#> 
#> $failure_reason
#> [1] NA
#> 
#> $data
#>     future    rate                time
#> 1 CRV-PERP 6.7e-05 2021-11-03 12:00:00
#> 2 XRP-PERP 5.1e-05 2021-11-03 12:00:00
#> 3 XRP-PERP 4.7e-05 2021-11-03 11:00:00
#> 4 CRV-PERP 7.2e-05 2021-11-03 11:00:00
#> 5 CRV-PERP 7.2e-05 2021-11-03 10:00:00
#> 6 XRP-PERP 5.3e-05 2021-11-03 10:00:00
#> 7 CRV-PERP 9.3e-05 2021-11-03 09:00:00
```
