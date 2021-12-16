
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rFTX

<!-- badges: start -->
<!-- badges: end -->

The goal of rFTX is to provide R interface for FTX REST API. The user is
required to have an FTX account as the account’s API key and secret key
is used in authentication. These can be created here:
<https://ftx.com/profile>. If you are using a subaccount, some functions
allow the name as an argument. This can be left out if not using
subaccounts.

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
#>  1 2810430189  1.55   250 buy   FALSE       2021-12-16 10:54:30
#>  2 2810430001  1.55   267 buy   FALSE       2021-12-16 10:54:26
#>  3 2810429890  1.55   153 buy   FALSE       2021-12-16 10:54:24
#>  4 2810429878  1.55   114 buy   FALSE       2021-12-16 10:54:24
#>  5 2810429512  1.55  1022 sell  FALSE       2021-12-16 10:54:22
#>  6 2810429511  1.55   998 sell  FALSE       2021-12-16 10:54:22
#>  7 2810429510  1.55     1 sell  FALSE       2021-12-16 10:54:22
#>  8 2810429509  1.55     5 sell  FALSE       2021-12-16 10:54:22
#>  9 2810429508  1.55   449 sell  FALSE       2021-12-16 10:54:22
#> 10 2810429319  1.55   162 buy   FALSE       2021-12-16 10:54:20
#> 11 2810429306  1.55   162 buy   FALSE       2021-12-16 10:54:20
#> 12 2810428515  1.55  5051 sell  FALSE       2021-12-16 10:54:16
#> 13 2810428185  1.55   336 buy   FALSE       2021-12-16 10:54:12
#> 14 2810428184  1.55    29 buy   FALSE       2021-12-16 10:54:12
#> 15 2810427769  1.55     5 sell  FALSE       2021-12-16 10:54:06
#> 16 2810427384  1.55   162 sell  FALSE       2021-12-16 10:54:00
#> 17 2810427383  1.55    43 sell  FALSE       2021-12-16 10:54:00
#> 18 2810427382  1.55    97 sell  FALSE       2021-12-16 10:54:00
#> 19 2810427230  1.55   300 sell  FALSE       2021-12-16 10:53:59
#> 20 2810427021  1.55   128 sell  FALSE       2021-12-16 10:53:58
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
#>   future        rate time               
#>   <chr>        <dbl> <dttm>             
#> 1 CRV-PERP -0.000045 2021-12-16 10:00:00
#> 2 XRP-PERP  0.000012 2021-12-16 10:00:00
#> 3 XRP-PERP -0.000003 2021-12-16 09:00:00
#> 4 CRV-PERP -0.000041 2021-12-16 09:00:00
#> 5 CRV-PERP -0.000036 2021-12-16 08:00:00
#> 6 XRP-PERP -0.000005 2021-12-16 08:00:00
```
