
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
devtools::install_github("andreskull/rFTX", build_vignettes = TRUE)
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
#> # A tibble: 20 x 6
#>            id price  size side  liquidation time               
#>         <dbl> <dbl> <dbl> <chr> <lgl>       <dttm>             
#>  1 2350709785  2.86   450 buy   FALSE       2021-11-05 11:20:55
#>  2 2350709777  2.86   720 buy   FALSE       2021-11-05 11:20:54
#>  3 2350709776  2.86   175 buy   FALSE       2021-11-05 11:20:54
#>  4 2350709775  2.86     3 buy   FALSE       2021-11-05 11:20:54
#>  5 2350709774  2.86    57 buy   FALSE       2021-11-05 11:20:54
#>  6 2350709773  2.86   421 buy   FALSE       2021-11-05 11:20:54
#>  7 2350709772  2.86   503 buy   FALSE       2021-11-05 11:20:54
#>  8 2350709771  2.86     3 buy   FALSE       2021-11-05 11:20:54
#>  9 2350709770  2.86     2 buy   FALSE       2021-11-05 11:20:54
#> 10 2350709769  2.86  1399 buy   FALSE       2021-11-05 11:20:54
#> 11 2350709768  2.86   600 buy   FALSE       2021-11-05 11:20:54
#> 12 2350709767  2.86  3250 buy   FALSE       2021-11-05 11:20:54
#> 13 2350709766  2.86    96 buy   FALSE       2021-11-05 11:20:54
#> 14 2350709765  2.86   836 buy   FALSE       2021-11-05 11:20:54
#> 15 2350709764  2.86  1045 buy   FALSE       2021-11-05 11:20:54
#> 16 2350709763  2.86   450 buy   FALSE       2021-11-05 11:20:54
#> 17 2350709762  2.86   440 buy   FALSE       2021-11-05 11:20:54
#> 18 2350709572  2.86    55 buy   FALSE       2021-11-05 11:20:52
#> 19 2350709405  2.86   422 sell  FALSE       2021-11-05 11:20:51
#> 20 2350709394  2.86   706 sell  FALSE       2021-11-05 11:20:51
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
#> # A tibble: 6 x 3
#>   future       rate time               
#>   <chr>       <dbl> <dttm>             
#> 1 CRV-PERP 0.000063 2021-11-05 11:00:00
#> 2 XRP-PERP 0.000032 2021-11-05 11:00:00
#> 3 CRV-PERP 0.000054 2021-11-05 10:00:00
#> 4 XRP-PERP 0.000017 2021-11-05 10:00:00
#> 5 XRP-PERP 0.000029 2021-11-05 09:00:00
#> 6 CRV-PERP 0.000067 2021-11-05 09:00:00
```
