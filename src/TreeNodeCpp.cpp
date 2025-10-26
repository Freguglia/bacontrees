#include "TreeNode.h"

#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

TreeNodeCpp::TreeNodeCpp(const std::string& path_) : path(path_), active(false), isLeaf(true) {

  std::string spath = path_;
  int n_dots = std::count(spath.begin(), spath.end(), '.');
  depth = n_dots;

  // compute parentPath
  if (n_dots > 0) {
    auto pos = spath.find_last_of('.');
    parentPath = spath.substr(0, pos);
  } else {
    parentPath = NA_STRING;
  }
}

void TreeNodeCpp::activate() { active = true; }
void TreeNodeCpp::deactivate() { active = false; }
bool TreeNodeCpp::isActive() const { return active; }
int TreeNodeCpp::getDepth() const { return depth; }

List TreeNodeCpp::getExtra() const { return extra; }
void TreeNodeCpp::setExtra(Rcpp::String name, SEXP value) { extra[name] = value; }

String TreeNodeCpp::getPath() const { return path; }
String TreeNodeCpp::getParentPath() const { return parentPath; }

CharacterVector TreeNodeCpp::getChildrenPaths() const { return childrenPaths; }

RCPP_EXPOSED_CLASS(TreeNodeCpp)

RCPP_MODULE(TreeNodeCpp_module) {
  class_<TreeNodeCpp>("TreeNodeCpp")
  .constructor<String>()
  .method("activate", &TreeNodeCpp::activate)
  .method("deactivate", &TreeNodeCpp::deactivate)
  .method("isActive", &TreeNodeCpp::isActive)
  .method("getExtra", &TreeNodeCpp::getExtra)
  .method("setExtra", &TreeNodeCpp::setExtra)
  .method("getDepth", &TreeNodeCpp::getDepth)
  .method("getPath", &TreeNodeCpp::getPath)
  .method("getParentPath", &TreeNodeCpp::getParentPath)
  .method("getChildrenPaths", &TreeNodeCpp::getChildrenPaths);
}
