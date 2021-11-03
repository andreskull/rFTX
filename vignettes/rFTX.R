## ---- eval = FALSE------------------------------------------------------------
#  # from GitHub
#  devtools::install_github("rFTX")
#  
#  # from CRAN
#  install.packages("rFTX")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages(tidyverse)
#  install.packages(digest)
#  install.packages(lubridate)
#  install.packages(stringi)
#  install.packages(assert)
#  install.packages(logging)
#  install.packages(httr)
#  

## ---- eval=FALSE, message=FALSE-----------------------------------------------
#  # loading the library
#  library(rFTX, quietly = T)
#  
#  # example use
#  obj <- ftx_coin_balances(key = "LR0RQT6bKjrUNh38eCw9jYC89VDAbRkCogAc_XAm",
#                           secret = "T4lPid48QtjNxjLUFOcUZghD7CUJ7sTVsfuvQZF2")
#  

