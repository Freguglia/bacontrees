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
#' @field counts Numeric vector to hold counts (active binding; validated on write).
#' @field extra List to hold extra information.
#' @importFrom stringr str_length str_sub str_split_1
TreeNode <- R6Class(
  "TreeNode",
  public = list(
    extra = list(),

#' @description
#' Initializaes the \code{TreeNode}
    initialize = function(path) {
      private$path <- path
      private$depth <- length(str_split_1(path, "\\.")) - 1
      if(private$depth > 0){
        parts <- str_split_1(private$path, "\\.")
        private$parentPath <- paste(parts[-length(parts)], collapse = ".")
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
  active = list(
    counts = function(value) {
      if(missing(value)) private$counts_
      else {
        if(!is.numeric(value)) stop("counts must be a numeric vector.")
        private$counts_ <- value
      }
    }
  ),
  private = list(
    path = character(0),
    parentPath = NA_character_,
    active = FALSE,
    depth = numeric(0),
    childrenPaths = character(0),
    isLeaf_ = TRUE,
    counts_ = integer(0)
  ))
