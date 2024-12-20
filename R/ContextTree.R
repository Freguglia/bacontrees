#' @title Context Tree R6 Class
#' @rdname ContextTree
#'
#' @param alphabet Either an object of class Alphabet or a character
#' vector with the symbols for the alphabet considered in the context tree.
#' @param path A string representing the path of a node.
#' @param idx A logical value. If \code{TRUE}, the function will return
#' the index (path) of the node as a string. If \code{FALSE}, returns
#' a list of nodes.
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
      }
      root <- TreeNode$new(path = "*")
      self$nodes[["*"]] <- root
      private$m <- length(private$Alphabet$symbols)
      root$counts <- rep(0, private$m)
      private$buildByDepth(maximalDepth)

      if(active == "root"){
        self$activateRoot()
      } else {
        self$activateMaximal()
      }

      private$growableNodes <- self$nodes[map_lgl(self$nodes,
                                                  function(node) node$isActive() & !node$isLeaf)]
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

    #' @return Returns a list of active nodes (leaf nodes of the active tree).
    getActiveNodes = function(idx = TRUE) {
      if(idx){
        names(self$nodes)[map_lgl(self$nodes, function(x) x$isActive())]
      } else {
        self$nodes[map_lgl(self$nodes, function(x) x$isActive())]
      }
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

    #' @return Returns the sibling nodes of the node in a given path.
    getSiblingNodes = function(path, idx = TRUE){
      if(path == "*"){
        if(idx) return(path)
        else return(self$root)
      }

      if(idx){
        names(self$nodes[self$nodes[[self$nodes[[path]]$getParentPath()]]$childrenIndex])
      } else {
        self$nodes[self$nodes[[self$nodes[[path]]$getParentPath()]]$childrenIndex]
      }
    },

    #' @description
        #' Replaces the node of a given path by its children in the active tree.
    growActive = function(path){
      node <- self$nodes[[path]]
      if(node$isActive() & !node$isLeaf){
        node$deactivate()
        for(child in self$nodes[node$childrenIndex]){
          child$activate()
        }
      } else {
        stop("Cannot grow a node that is not active or is a leaf node.")
      }
    },

    #' @return Returns a list of active nodes that have
    #' children that can be activated.
    getGrowableNodes = function(idx = TRUE){
      if(idx){
        names(private$growableNodes)
      } else {
        private$growableNodes
      }
    },

    #' @description
        #' Sets data for the Context Tree by setting the counts of occurrences
        #' of each symbol of the alphabet within each context (node) of the tree.
    #' @param Sequence A "Sequence" object to be set as data for the context tree.
    setData = function(Sequence) {
      if(private$hasData){
        warning("This Context Tree already had data. Overwriting previous data with the new one.")
      }
      self$data <- Sequence
      for(sequence_vec in Sequence$data){
        private$fillData(sequence_vec)
      }
      private$hasData <- TRUE
    },

    #' @description
        #' Prints the current active Context Tree and the counts for
        #' each context.
    print = function() {
      cat("Active Context Tree:\n")
      to_print <- list(self$root)
      while(length(to_print) > 0){
        node <- to_print[[1]]
        nodePath <- node$getPath()
        output_string <- gsub(".", " ", substr(nodePath, 1, nchar(nodePath) - 2))
        output_string <- paste0(output_string, substr(nodePath, nchar(nodePath) - 1, nchar(nodePath)))
        cat(glue("{output_string}: {paste0(node$counts, collapse = ' ')}"))
        cat("\n")
        to_print <- to_print[-1]
        if(!node$isActive()){
          to_print <- c(self$nodes[node$childrenIndex],to_print)
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
      if (!self$nodeExists(path)) {
        node <- TreeNode$new(path)
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
            self$nodes[[path]]$childrenIndex <- c(self$nodes[[path]]$childrenIndex,
                                                  length(self$nodes))
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
          node <- self$nodes[[node$childrenIndex[sequence_vector[t-dt]]]]
          dt <- dt + 1
        }
      }
    },

    growableNodes = list()
  )
)
