.onLoad <- function(libname, pkgname) {
  Rcpp::loadModule("TreeNodeCpp_module", what = TRUE)
}
