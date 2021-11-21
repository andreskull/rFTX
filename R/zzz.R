pkg.env <- new.env(parent = emptyenv())
pkg.env$base_url <- NA

.onLoad <- function(libname, pkgname) {
  base_url <- ftx_init()
}