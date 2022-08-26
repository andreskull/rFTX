## ---- eval = FALSE------------------------------------------------------------
#  # from GitHub
#  devtools::install_github("andreskull/rFTX", build_vignettes = TRUE)
#  
#  # from CRAN
#  install.packages("rFTX")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages(digest)
#  install.packages(lubridate)
#  install.packages(logging)
#  install.packages(httr)
#  

## ---- message=FALSE-----------------------------------------------------------
# loading the library
library(rFTX, quietly = T)

# example use
ftx_trades(key = "", secret = "", market = "AAPL/USD")


