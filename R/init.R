#' @keywords internal
.onLoad <- function(libname, pkgname) {

  # run the .setup_fonts() function on package load
  .setup_fonts()
}
