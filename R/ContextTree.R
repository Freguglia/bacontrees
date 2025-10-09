#' @title ContextTree R6 Class
#'
#' @description
#' The `ContextTree` class represents a variable-length Markov context tree, supporting construction, manipulation, and data assignment for context tree models. It manages nodes, active/inactive states, and provides methods for growing, pruning, and validating the tree.
#'
#' @details
#' This class is the core data structure for context tree modeling, supporting both root and maximal initialization, and efficient management of tree structure and data.
#'
#' @param alphabet Either an object of class Alphabet or a character vector with the symbols for the alphabet considered in the context tree.
#' @param path A string representing the path of a node.
#' @param idx A logical value. If TRUE, the function will return the index (path) of the node as a string. If FALSE, returns a list of nodes.
#' @param code The tree code for the tree to be activated.
#' @param data A `Sequence` object, a character vector with a single observed chain or a list of vectors of observed chains to be set as data for the context tree.
#'
#' @examples
#' tree <- ContextTree$new(alphabet = c("a", "b", "c"), maximalDepth = 3)
#' tree$activateMaximal()
#' tree$setData(list(rep("a", 10), rep("b", 10)))
#' print(tree)
#'
#' @importFrom purrr map_chr map_lgl
#' @importFrom glue glue
#' @export
ContextTree <- R6Class(
  "ContextTree",
  public = list(
    #' @field nodes List of nodes from a context tree (both active and non-active).
    nodes = list(),

    #' @field data A list of observed sequences of data.
    data = NULL,

    #' @param maximalDepth Depth of the maximal tree considered.
    #' @param active Either "root" or "maximal" to indicate which nodes
    #' should be initialized as active.
    initialize = function(alphabet = NULL, maximalDepth = 3, active = "root") {
      if("Alphabet" %in% class(alphabet)){
        private$Alphabet <- alphabet
      } else if("character" %in% class(alphabet)){
        private$Alphabet <- Alphabet$new(alphabet)
      } else {
        stop("alphabet must be either a character vector or an Alphabet object.")
      }
      root <- TreeNode$new(path = "*")
      self$nodes[["*"]] <- root
      root$setChildrenPaths(glue("*.{private$Alphabet$symbols}"))
      private$m <- length(private$Alphabet$symbols)
      root$counts <- rep(0, private$m)
      private$buildByDepth(maximalDepth)

      if(active == "root"){
        self$activateRoot()
      } else {
        self$activateMaximal()
      }

      private$growableNodes <- names(self$nodes[map_lgl(self$nodes,
                                                        function(node) node$isActive() & !node$isLeaf)])
    },

    #' @return Returns the Context Tree root node.
    root = function(){
      self$nodes[["*"]]
    },

    #' @return Returns a logical value incating whether the Context Tree is
    #' valid.
    validate = function() {
      for (path in names(self$nodes)) {
        node <- self$nodes[[path]]
        if (!node$isLeaf) {
          children_paths <- names(self$nodes)[startsWith(names(self$nodes), paste0(path, "."))]
          children_depths <- sapply(children_paths, function(p) length(str_split_1(p, "\\.")))
          expected_depth <- length(str_split_1(path, "\\.")) + 1
          children_paths <- children_paths[children_depths == expected_depth]
          if (length(children_paths) != private$m) {
            return(FALSE)
          }
        }
      }
      return(TRUE)
    },

    #' @return Returns the alphabet related to the Context Tree.
    getAlphabet = function(){
      private$Alphabet
    },

    #' @return Returns a list of active nodes (leaf nodes of the active tree).
    getActiveNodes = function(idx = TRUE) {
      vec <- check_active(self$nodes)
      if(idx){
        names(self$nodes)[vec]
      } else {
        self$nodes[vec]
      }
    },

    #' @return Returns a character value representing the active tree.
    activeTreeCode = function(){
      compress_logical(check_active(self$nodes))
    },

    #' @description
    #' Sets the active tree to be the one containing only the root node.
    activateRoot = function() {
      for(node in self$nodes) {
        if(node$getPath() == "*") {
          node$activate()
        } else {
          node$deactivate()
        }
      }
    },

    #' @description
    #' Activates the leaf nodes of the maximal Context Tree.
    activateMaximal = function() {
      for(node in self$nodes) {
        if(node$isLeaf) {
          node$activate()
        } else {
          node$deactivate()
        }
      }
      private$prunableNodes <- self$getActiveNodes()
    },

    #' @description
    #' Sets the active tree to be the one corresponding to a tree code obtained
    #' from the `activeTreeCode` method.
    #'
    #' @param code The tree code for the tree to be activated.
    activateByCode = function(code) {
      n_nodes <- length(self$nodes)
      active_nodes <- decompress_logical(code, n_nodes)
      for(i in seq_len(n_nodes)){
        if(active_nodes[i]){
          self$nodes[[i]]$activate()
        } else {
          self$nodes[[i]]$deactivate()
        }
      }
      # This still needs some checking to make sure the active tree is
      # a valid one.
    },

    #' @return Returns the leaf nodes of the maximal Context Tree (regardless of
    #' the current active tree).
    getLeaves = function(idx = TRUE) {
      if(idx){
        names(self$nodes)[map_lgl(self$nodes, function(x) x$isLeaf)]
      } else {
        self$nodes[map_lgl(self$nodes, function(x) x$isLeaf)]
      }
    },

    #' @return Returns the inner nodes nodes of the active Context Tree.
    #' Inner nodes are nodes that are in the path between the root (including)
    #' and active nodes.
    getInnerNodes = function(idx = TRUE) {
      innerNodes <- character(0)
      currentNodes <- self$getActiveNodes(idx = FALSE)
      parentNodes <- unique(map_chr(currentNodes, function(node) node$getParentPath()))
      currentNodes <- parentNodes[!is.na(parentNodes)]
      innerNodes <- c(innerNodes, currentNodes)
      while(length(currentNodes) > 0){
        parentNodes <- unique(map_chr(currentNodes, function(node) self$nodes[[node]]$getParentPath()))
        currentNodes <- parentNodes[!is.na(parentNodes)]
        innerNodes <- unique(c(innerNodes, currentNodes))
      }
      return(innerNodes)
    },

    #' @return TRUE if a node with a given path exists in the Context Tree.
    nodeExists = function(path) path %in% names(self$nodes),

    #' @return Returns the parent node of the node in a given path.
    getParentNode = function(path, idx = TRUE){
      if(idx){
        self$nodes[[path]]$getParentPath()
      } else {
        self$nodes[[self$nodes[[path]]$getParentPath()]]
      }
    },

    #' @return Returns the parent node of the node in a given path.
    getChildrenNodes = function(path, idx = TRUE){
      if(idx){
        self$nodes[[path]]$getChildrenPaths()
      } else {
        self$nodes[self$nodes[[path]]$getChildrenPaths()]
      }
    },

    #' @return Returns the sibling nodes of the node in a given path.
    getSiblingNodes = function(path, idx = TRUE){
      if(path == "*"){
        if(idx) return(path)
        else return(list(self$root()))
      }

      if(idx){
        names(self$nodes[self$nodes[[self$nodes[[path]]$getParentPath()]]$getChildrenPaths()])
      } else {
        self$nodes[self$nodes[[self$nodes[[path]]$getParentPath()]]$getChildrenPaths()]
      }
    },

    #' @description
    #' Replaces the node of a given path by its children in the active tree.
    growActive = function(path){
      node <- self$nodes[[path]]
      siblings <- self$getSiblingNodes(path)
      if(node$isActive() & !node$isLeaf){
        node$deactivate()
        private$growableNodes <- setdiff(private$growableNodes, node$getPath())
        private$prunableNodes <- setdiff(private$prunableNodes, siblings)
        for(child in self$nodes[node$getChildrenPaths()]){
          child$activate()
          if(!child$isLeaf){
            private$growableNodes <- c(private$growableNodes, child$getPath())
          }
          private$prunableNodes <- c(private$prunableNodes, child$getPath())
        }
      } else {
        stop("Cannot grow a node that is not active or is a leaf node.")
      }
    },

    #' @description
    #' Replaces the node of a given path and its siblings by the parent node
    #' in the active tree.
    pruneActive = function(path){
      if(path %in% private$prunableNodes){
        siblings <- self$getSiblingNodes(path, idx = TRUE)
        private$growableNodes <- setdiff(private$growableNodes, siblings)
        private$prunableNodes <- setdiff(private$prunableNodes, siblings)
        parent <- self$getParentNode(path, idx = TRUE)
        private$growableNodes <- c(private$growableNodes, parent)
        for(sibling in siblings){
          self$nodes[[sibling]]$deactivate()
        }
        self$nodes[[parent]]$activate()
        active_parent_siblings <- map_lgl(self$getSiblingNodes(parent, idx = FALSE), function(x) x$isActive())
        if(length(active_parent_siblings) == 1){
          private$prunableNodes <- character(0)
        } else if(all(active_parent_siblings)){
          private$prunableNodes <- c(private$prunableNodes, self$getSiblingNodes(parent))
        }
      } else {
        stop("Specified node is not prunable within the active context tree.")
      }
    },

    #' @return Returns a list of paths of active nodes that have
    #' children that can be activated.
    getGrowableNodes = function(){
      private$growableNodes
    },

    #' @return Returns a list of paths of active nodes that have
    #' all siblings active.
    getPrunableNodes = function(){
      private$prunableNodes
    },

    #' @description
    #' Sets data for the Context Tree by setting the counts of occurrences
    #' of each symbol of the alphabet within each context (node) of the tree.
    #' @param data A `Sequence` object, a character vector with a single
    #' observed chain or a list of vectors of observed chains
    #' to be set as data for the context tree.
    setData = function(data) {
      if(private$hasData){
        warning("This Context Tree already had data. Overwriting previous data with the new one.")
      }
      if("Sequence" %in% class(data)){
        self$data <- data
      } else {
        self$data <- Sequence$new(data, alphabet = private$Alphabet)
      }
      for(sequence_vec in self$data$data){
        private$fillData(sequence_vec)
      }
      private$hasData <- TRUE
    },

    #' @description
    #' Prints the current active Context Tree and the counts for each context.
    print = function() {
      cat("Active Context Tree:\n")
      to_print <- list(self$root())
      first_symbol <- private$Alphabet$symbols[1]
      last_symbol <- private$Alphabet$symbols[private$m]
      while(length(to_print) > 0){
        node <- to_print[[1]]
        nodePath <- node$getPath()
        nodePath <- str_split(nodePath, "\\.")[[1]]
        output_string <- nodePath
        if(length(nodePath) > 1){
          output_string[length(nodePath) - 1] <- ifelse(nodePath[length(nodePath) ] == last_symbol,
                                                     "`-", "|-")
         if(length(nodePath) > 2){
           output_string[1:(length(nodePath) - 2)] <- ifelse(nodePath[2:(length(nodePath) - 1)] == last_symbol,
                                                         "  ", "| ")
         }
          output_string <- paste0(output_string, collapse = "")
        } else {
          output_string <- "*"
        }
        if(private$hasData){
          cat(glue("{output_string}: {paste0(node$counts, collapse = ' ')}"))
        } else {
          cat(glue("{output_string}"))
        }
        cat("\n")
        to_print <- to_print[-1]
        if(!node$isActive()){
          to_print <- c(self$nodes[node$getChildrenPaths()],to_print)
        }
      }
    }
  ),
  private = list(
    m = 0,
    Alphabet = NULL,
    buildByDepth = function(depth) {
      for (i in seq_len(depth)) {
        leaves <- self$getLeaves(TRUE)
        for(leaf in leaves){
          private$addChildren(leaf)
        }
      }
    },

    addNode = function(path) {
      symbols <- str_split_1(path, "\\.")[-1]
      children_paths <- glue("{path}.{private$Alphabet$symbols}")
      if (!self$nodeExists(path)) {
        node <- TreeNode$new(path)
        node$setChildrenPaths(children_paths)
        self$nodes[[path]] <- node
        return(node)
      }
      return(NULL)
    },

    addChildren = function(path) {
      if (self$nodeExists(path)) {
        children_paths <- glue("{path}.{private$Alphabet$symbols}")
        if (self$nodes[[path]]$isLeaf)
          for (child_path in children_paths) {
            private$addNode(child_path)
            self$nodes[[child_path]]$counts <- rep(0, private$m)
          }
        self$nodes[[path]]$isLeaf <- FALSE
      } else {
        stop(glue("Cannot add children to {path} because it is not a node."))
      }
    },
    hasData = FALSE,
    fillData = function(sequence_vector) {
      for(t in seq_along(sequence_vector)){
        current_symbol <- sequence_vector[t]
        node <- self$root()
        dt <- 1
        while(!is.null(node) & (t-dt) > 0){
          node$counts[current_symbol] <- node$counts[current_symbol] + 1
          node <- self$nodes[[node$getChildrenPaths()[sequence_vector[t-dt]]]]
          dt <- dt + 1
        }
      }
    },

    growableNodes = character(0),
    prunableNodes = character(0)
  )
)
