#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string compress_logical_cpp(LogicalVector logical_vec, int chunk_size) {
  int n = logical_vec.size();
  std::string result;

  for (int i = 0; i < n; i += chunk_size) {
    int end = std::min(i + chunk_size, n);
    int decimal = 0;

    for (int j = i; j < end; ++j) {
      decimal = (decimal << 1) | logical_vec[j];
    }

    result += static_cast<char>(decimal + 65);
  }

  return result;
}
