#' @importFrom purrr map_chr map_lgl
#' @importFrom glue glue
#' @export
ContextTree <- R6Class(
  "ContextTree",
  public = list(
    nodes = list(),
    Alphabet = NULL,
    root = NULL,
    data = NULL,
    m = 0,
    initialize = function(Alphabet = NULL, maximalDepth = 3, active = "root") {
      if("Alphabet" %in% class(Alphabet)){
        self$Alphabet <- Alphabet
      } else if(class(Alphabet) == "character"){
        self$Alphabet <- bacontrees:::Alphabet$new(Alphabet)
      }
      root <- TreeNode$new(path = "*")
      self$nodes[[root$getPath()]] <- root
      self$root <- root
      self$m <- length(self$Alphabet$symbols)
      root$counts <- rep(0, self$m)
      private$buildByDepth(maximalDepth)

      if(active == "root"){
        self$activateRoot()
      } else {
        self$activateMaximal()
      }

      private$growableNodes <- self$nodes[map_lgl(self$nodes,
                                                  function(node) node$isActive() & !node$isLeaf)]
    },

    validate = function() {
      for (path in names(self$nodes)) {
        node <- self$nodes[[path]]
        if (!node$isLeaf) {
          children_paths <- names(self$nodes)[startsWith(names(self$nodes), paste0(path, "."))]
          children_depths <- sapply(children_paths, function(p) length(str_split_1(p, "\\.")))
          expected_depth <- length(str_split_1(path, "\\.")) + 1
          children_paths <- children_paths[children_depths == expected_depth]
          if (length(children_paths) != self$m) {
            return(FALSE)
          }
        }
      }
      return(TRUE)
    },

    getActiveNodes = function(idx = TRUE) {
      if(idx){
        names(self$nodes)[map_lgl(self$nodes, function(x) x$isActive())]
      } else {
        self$nodes[map_lgl(self$nodes, function(x) x$isActive())]
      }
    },

    activateRoot = function() {
      for(node in self$nodes) {
        if(node$getPath() == "*") {
          node$activate()
        } else {
          node$deactivate()
        }
      }
    },

    activateMaximal = function() {
      for(node in self$nodes) {
        if(node$isLeaf) {
          node$activate()
        } else {
          node$deactivate()
        }
      }
    },

    getLeaves = function(idx = TRUE) {
      if(idx){
        names(self$nodes)[map_lgl(self$nodes, function(x) x$isLeaf)]
      } else {
        self$nodes[map_lgl(self$nodes, function(x) x$isLeaf)]
      }
    },

    nodeExists = function(path) path %in% names(self$nodes),

    getParentNode = function(path, idx = TRUE){
      if(idx){
        self$nodes[[path]]$getParentPath()
      } else {
        self$nodes[[self$nodes[[path]]$getParentPath()]]
      }
    },

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

    growActive = function(nodePath){
      node <- self$nodes[[nodePath]]
      if(node$isActive() & !node$isLeaf){
        node$deactivate()
        for(child in self$nodes[node$childrenIndex]){
          child$activate()
        }
      } else {
        stop("Cannot grow a node that is not active or is a leaf node.")
      }
    },

    getGrowableNodes = function(idx = TRUE){
      if(idx){
        names(private$growableNodes)
      } else {
        private$growableNodes
      }
    },

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
        children_paths <- glue("{path}.{self$Alphabet$symbols}")
        if (self$nodes[[path]]$isLeaf)
          for (child_path in children_paths) {
            private$addNode(child_path)
            self$nodes[[path]]$childrenIndex <- c(self$nodes[[path]]$childrenIndex,
                                                  length(self$nodes))
            self$nodes[[child_path]]$counts <- rep(0, self$m)
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
        node <- self$root
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
