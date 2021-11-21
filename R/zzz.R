#' @importFrom utils assignInMyNamespace

base_url <- NA

.onLoad <- function(libname, pkgname) {
  assignInMyNamespace("base_url", ftx_init())
}