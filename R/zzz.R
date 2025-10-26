.onLoad <- function(libname, pkgname) {
  Rcpp::loadModule("TreeNodeCpp_module", what = TRUE)
  Rcpp::loadModule("ContextTreeCpp_module", what = TRUE)
}
