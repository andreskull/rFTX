
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

You can install the released version of rFTX from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rFTX")
```

And the development version from [GitHub](https://github.com/) with:

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
#> 1  2325707053 2.66555    5  buy       FALSE 2021-11-03 10:41:57
#> 2  2325707052 2.66555  393  buy       FALSE 2021-11-03 10:41:57
#> 3  2325706543 2.66550    3 sell       FALSE 2021-11-03 10:41:50
#> 4  2325706311 2.66555  391  buy       FALSE 2021-11-03 10:41:47
#> 5  2325706108 2.66530    5  buy       FALSE 2021-11-03 10:41:47
#> 6  2325705833 2.66530  240  buy       FALSE 2021-11-03 10:41:44
#> 7  2325705716 2.66500 1414  buy       FALSE 2021-11-03 10:41:42
#> 8  2325705693 2.66485    5  buy       FALSE 2021-11-03 10:41:42
#> 9  2325705580 2.66485   15  buy       FALSE 2021-11-03 10:41:41
#> 10 2325705552 2.66455   65  buy       FALSE 2021-11-03 10:41:40
#> 11 2325705544 2.66455    5  buy       FALSE 2021-11-03 10:41:40
#> 12 2325705014 2.66490    5  buy       FALSE 2021-11-03 10:41:37
#> 13 2325705013 2.66490   78  buy       FALSE 2021-11-03 10:41:37
#> 14 2325704948 2.66485    3 sell       FALSE 2021-11-03 10:41:37
#> 15 2325704933 2.66490  392  buy       FALSE 2021-11-03 10:41:37
#> 16 2325704911 2.66435    6  buy       FALSE 2021-11-03 10:41:37
#> 17 2325704910 2.66435    5  buy       FALSE 2021-11-03 10:41:37
#> 18 2325704612 2.66435   55  buy       FALSE 2021-11-03 10:41:34
#> 19 2325704520 2.66425   62 sell       FALSE 2021-11-03 10:41:33
#> 20 2325704456 2.66440   33  buy       FALSE 2021-11-03 10:41:32
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
#> 1 CRV-PERP 7.2e-05 2021-11-03 10:00:00
#> 2 XRP-PERP 5.3e-05 2021-11-03 10:00:00
#> 3 CRV-PERP 9.3e-05 2021-11-03 09:00:00
#> 4 XRP-PERP 5.7e-05 2021-11-03 09:00:00
#> 5 XRP-PERP 6.1e-05 2021-11-03 08:00:00
#> 6 CRV-PERP 9.9e-05 2021-11-03 08:00:00
```
