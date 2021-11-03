
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

## Example

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
#> 1  2325644746 2.69270   66  buy       FALSE 2021-11-03 10:33:07
#> 2  2325644745 2.69200  340  buy       FALSE 2021-11-03 10:33:07
#> 3  2325644744 2.69195    5  buy       FALSE 2021-11-03 10:33:07
#> 4  2325644743 2.69195  520  buy       FALSE 2021-11-03 10:33:07
#> 5  2325644722 2.69095    3 sell       FALSE 2021-11-03 10:33:07
#> 6  2325644649 2.69090    4  buy       FALSE 2021-11-03 10:33:06
#> 7  2325644648 2.69080  112  buy       FALSE 2021-11-03 10:33:06
#> 8  2325644543 2.69090    4  buy       FALSE 2021-11-03 10:33:06
#> 9  2325644495 2.69055   62 sell       FALSE 2021-11-03 10:33:05
#> 10 2325644291 2.69065  298  buy       FALSE 2021-11-03 10:33:03
#> 11 2325644263 2.69040    1  buy       FALSE 2021-11-03 10:33:02
#> 12 2325644257 2.69040    4  buy       FALSE 2021-11-03 10:33:02
#> 13 2325644151 2.68955  520  buy       FALSE 2021-11-03 10:33:02
#> 14 2325644061 2.68900   19  buy       FALSE 2021-11-03 10:33:01
#> 15 2325643986 2.68865  690  buy       FALSE 2021-11-03 10:33:00
#> 16 2325643865 2.68915   82 sell       FALSE 2021-11-03 10:32:59
#> 17 2325643864 2.68915  209 sell       FALSE 2021-11-03 10:32:59
#> 18 2325643855 2.68915  209 sell       FALSE 2021-11-03 10:32:59
#> 19 2325643815 2.68915  277 sell       FALSE 2021-11-03 10:32:58
#> 20 2325643814 2.68945   30 sell       FALSE 2021-11-03 10:32:58
```
