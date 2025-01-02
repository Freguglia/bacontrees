#' @title TreeNode Class
#'
#' @description This class represents a tree node with a path, depth,
#' and various methods
#' for managing its state and validating its path.
#'
#' @importFrom stringr str_length str_sub str_split_1
TreeNode <- R6Class(
  "TreeNode",
  public = list(
    #' @field childrenIndex Character vector to hold indices of child nodes.
    childrenIndex = integer(0),

    #' @field counts Numeric vector to hold counts.
    counts = integer(0),

    #' @field isLeaf Logical value indicating if the node is a leaf.
    isLeaf = TRUE,

    #' @field extra List to hold extra information.
    extra = list(),

    #' Initialize a TreeNode
    #'
    #' This method initializes a tree node with a given path, calculates its depth,
    #' and sets the parent path if applicable.
    #'
    #' @param path A character string representing the path of the node.
    #' @return A new instance of the TreeNode class.
    initialize = function(path) {
      private$path <- path
      private$depth <- length(str_split_1(path, "\\.")) - 1
      if(private$depth > 0){
        private$parentPath <- str_sub(private$path, end = -3)
      }
    },

    #' Print the path of the node
    #'
    #' This method prints the path of the node to the console.
    #'
    #' @param ... Additional arguments passed to the `cat` function.
    #' @return None. Prints the path to the console.
    print = function(...) {
      cat(private$path)
    },

    #' Activate the node
    #'
    #' This method sets the node's active status to TRUE.
    #'
    #' @return None.
    activate = function() {private$active <- TRUE},

    #' Deactivate the node
    #'
    #' This method sets the node's active status to FALSE.
    #'
    #' @return None.
    deactivate = function() {private$active <- FALSE},

    #' Check if the node is active
    #'
    #' This method returns the active status of the node.
    #'
    #' @return Logical value indicating if the node is active.
    isActive = function() {private$active},

    #' Get the depth of the node
    #'
    #' This method returns the depth of the node.
    #'
    #' @return Numeric value representing the depth of the node.
    getDepth = function() {private$depth},

    #' Get the path of the node
    #'
    #' This method returns the path of the node.
    #'
    #' @return Character string representing the path of the node.
    getPath = function() {private$path},

    #' Get the parent path of the node
    #'
    #' This method returns the parent path of the node.
    #'
    #' @return Character string representing the parent path of the node.
    getParentPath = function() {private$parentPath},

    #' Validate the node path against an alphabet
    #'
    #' This method validates the node's path by checking if all elements of the path
    #' (excluding the first element) are in the provided alphabet.
    #'
    #' @param Alphabet An object containing a `symbols` vector to validate against.
    #' @return Logical value indicating if the path is valid.
    validatePath = function(Alphabet) {
      s <- str_split_1(private$path, "\\.")[-1]
      return(all(s %in% Alphabet$symbols))
    }
  ),
  private = list(
    path = character(0),
    parentPath = NULL,
    active = FALSE,
    depth = numeric(0)
  ))
