#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib bacontrees, .registration = TRUE
## usethis namespace: end
NULL

utils::globalVariables(c("tree_code", "tree_contexts", "tree", "prob"))
