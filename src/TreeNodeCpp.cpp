#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

class TreeNodeCpp {
private:
  String path;
  String parentPath;
  bool active;
  int depth;
  CharacterVector childrenPaths;
  std::vector<int> counts;
  bool isLeaf;
  List extra;

public:
  TreeNodeCpp(String path_)
    : path(path_), active(false), isLeaf(true) {

    std::string spath = path_;
    int n_dots = std::count(spath.begin(), spath.end(), '.');
    depth = n_dots;

    // compute parentPath
    if (n_dots > 0) {
      auto pos = spath.find_last_of('.');
      parentPath = spath.substr(0, pos);
    } else {
      parentPath = NA_STRING;  // now safe with Rcpp::String
    }
  }

  void activate() { active = true; }
  void deactivate() { active = false; }
  bool isActive() const { return active; }
  int getDepth() const { return depth; }

  String getPath() const { return path; }
  String getParentPath() const { return parentPath; }

  CharacterVector getChildrenPaths() const { return childrenPaths; }

  void setChildrenPaths(CharacterVector childrenPaths_) {
    if (childrenPaths.size() != 0)
      stop("Attempting to set children node paths multiple times.");
    childrenPaths = childrenPaths_;
  }
};

RCPP_MODULE(TreeNodeCpp_module) {
  class_<TreeNodeCpp>("TreeNodeCpp")
  .constructor<String>()
  .method("activate", &TreeNodeCpp::activate)
  .method("deactivate", &TreeNodeCpp::deactivate)
  .method("isActive", &TreeNodeCpp::isActive)
  .method("getDepth", &TreeNodeCpp::getDepth)
  .method("getPath", &TreeNodeCpp::getPath)
  .method("getParentPath", &TreeNodeCpp::getParentPath)
  .method("getChildrenPaths", &TreeNodeCpp::getChildrenPaths)
  .method("setChildrenPaths", &TreeNodeCpp::setChildrenPaths);
}
