#include "TreeNode.h"
#include <Rcpp.h>
#include <unordered_map>
#include <memory>
#include <string>
#include <queue>
using namespace Rcpp;

class ContextTreeCpp {
private:
  int treeDepth;
  CharacterVector Alphabet;
  std::unordered_map<std::string, std::shared_ptr<TreeNodeCpp>> nodes;

public:
  ContextTreeCpp(CharacterVector alphabet_, int depth_)
    : treeDepth(depth_), Alphabet(alphabet_) {
    std::cout << "created" << std::endl;
    nodes["*"] = std::make_shared<TreeNodeCpp>("*");

    std::queue<std::string> q;
    q.push("*");

    while(!q.empty()) {
      std::string current = q.front();
      q.pop();

      int currentDepth = std::count(current.begin(), current.end(), '.');

      if(currentDepth < treeDepth) {
        for(int i = 0; i < Alphabet.size(); i++) {
          std::string symbol = as<std::string>(Alphabet[i]);
          std::string childPath = current + "." + symbol;

          if(nodes.find(childPath) == nodes.end()) {
            nodes[childPath] = std::make_unique<TreeNodeCpp>(childPath);
          }

          q.push(childPath);
        }
      }
    }
  }


  int getDepth() const {return treeDepth;}

  TreeNodeCpp* getRoot() const {return nodes.find("*")->second.get();}

  TreeNodeCpp* getNode(const std::string& path) const {
    auto it = nodes.find(path);
    if (it != nodes.end()) {
      return it->second.get();
    }
    stop("Node with path '%s' not found.", path);
  }

  void addNode(const std::string& path){
    if (nodes.find(path) == nodes.end()) {
      nodes[path] = std::make_shared<TreeNodeCpp>(path);
    }
  }

  bool nodeExists(const std::string& path) const {
    return nodes.find(path) != nodes.end();
  }
};

RCPP_MODULE(ContextTreeCpp_module) {
  class_<ContextTreeCpp>("ContextTreeCpp")
  .constructor<CharacterVector, int>()
  .method("getDepth", &ContextTreeCpp::getDepth)
  .method("getRoot", &ContextTreeCpp::getRoot)
  .method("getNode", &ContextTreeCpp::getNode)
  .method("nodeExists", &ContextTreeCpp::nodeExists)
  .method("addNode", &ContextTreeCpp::addNode);
}
