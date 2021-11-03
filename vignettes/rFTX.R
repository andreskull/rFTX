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

## ---- message=FALSE-----------------------------------------------------------
# loading the library
library(rFTX, quietly = T)

# example use
obj <- ftx_send_request(method = "GET", path = "/api/funding_rates", key = "", secret = "")


