#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector check_active(List nodes) {
  int n = nodes.size();
  LogicalVector result(n);

  for (int i = 0; i < n; ++i) {
    Rcpp::Environment node = nodes[i];
    Rcpp::Function isActive = node["isActive"];
    result[i] = Rcpp::as<bool>(isActive());
  }

  return result;
}
