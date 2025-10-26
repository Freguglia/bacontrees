#ifndef TREENODE_H
#define TREENODE_H

#include <Rcpp.h>
#include <string>

class TreeNodeCpp {
private:
  Rcpp::String path;
  Rcpp::String parentPath;
  bool active;
  int depth;
  Rcpp::CharacterVector childrenPaths;
  std::vector<int> counts;
  bool isLeaf;
  Rcpp::List extra;

public:
  TreeNodeCpp(const std::string& path_);
  Rcpp::String getPath() const;
  int getDepth() const;
  void activate();
  void deactivate();
  bool isActive() const;
  Rcpp::List getExtra() const;
  void setExtra(Rcpp::String name, SEXP value);
  Rcpp::String getParentPath() const;
  Rcpp::CharacterVector getChildrenPaths() const;
};

#endif
