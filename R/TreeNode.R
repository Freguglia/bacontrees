#' @title TreeNode Class
#'
#' @description This class represents a tree node with a path, depth,
#' and various methods
#' for managing its state and validating its path.
#'
#' @param path A character string representing the path of the node.
#' @param Alphabet An \code{Alphabet} object containing a `symbols` vector to validate against.
#' @param childrenPaths vector of paths for the children of the node.
#' @param ... Additional arguments passed to the `cat` function.
#'
#' @field counts Numeric vector to hold counts.
#' @field extra List to hold extra information.
#' @importFrom stringr str_length str_sub str_split_1
TreeNode <- R6Class(
  "TreeNode",
  public = list(
    counts = integer(0),
    extra = list(),

#' @description
#' Initializaes the \code{TreeNode}
    initialize = function(path) {
      private$path <- path
      private$depth <- length(str_split_1(path, "\\.")) - 1
      if(private$depth > 0){
        private$parentPath <- str_sub(private$path, end = -3)
      }
    },

#' @description Print the path of the node
    print = function(...) {
      cat(private$path)
    },

#' @description
#' Activate the node.
#'
#' This method sets the node's active status to TRUE.
    activate = function() {private$active <- TRUE},

#' @description
#' Deactivate the node.
#'
#' This method sets the node's active status to FALSE.
    deactivate = function() {private$active <- FALSE},

#' @return Logical value indicating if the node is active.
    isActive = function() {private$active},

#' @return Integer value representing the depth of the node.
    getDepth = function() {private$depth},

#' @return Returns a logical value indicating whether the node is a leaf
#' (of the maximal tree, not the active tree).
    isLeaf = function() {private$isLeaf_},

#' @return Character string representing the path of the node.
    getPath = function() {private$path},

#' @return Character string representing the parent path of the node.
    getParentPath = function() {private$parentPath},

#' @return Character string representing the paths for the children
#' of a node.
    getChildrenPaths = function() {private$childrenPaths},

#' @description
#' Sets the paths for children of a node.
    setChildrenPaths = function(childrenPaths) {
      if(length(private$childrenPaths) == 0){
        private$childrenPaths <- childrenPaths
        private$isLeaf_ <- FALSE
      } else {
        stop("Attempting to set children node paths multiple times.")
      }
    },

#' @description Validate the node path against an alphabet
#'
#' This method validates the node's path by checking if all elements of the path
#' (excluding the first element) are in the provided alphabet.
#'
#' @return Logical value indicating if the path is valid.
    validatePath = function(Alphabet) {
      s <- str_split_1(private$path, "\\.")[-1]
      return(all(s %in% Alphabet$symbols))
    }
  ),
  private = list(
    path = character(0),
    parentPath = NA_character_,
    active = FALSE,
    depth = numeric(0),
    childrenPaths = character(0),
    isLeaf_ = TRUE
  ))
