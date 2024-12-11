#' @export
ContextTree <- R6Class(
  "ContextTree",
  public = list(
    nodes = list(),
    Alphabet = NULL,
    root = NULL,
    data = NULL,
    m = 0,
    initialize = function(Alphabet = NULL) {
      self$Alphabet <- Alphabet
      root <- TreeNode$new(path = "*")
      self$nodes[[root$getPath()]] <- root
      self$root <- root
      self$m <- length(self$Alphabet$symbols)
      root$counts <- rep(0, self$m)
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

    getActiveNodes = function() {
      unname(map_chr(bt$nodes[map_lgl(bt$nodes, ~.x$isActive())], ~.x$getPath()))
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

    getLeaves = function() {
      return(names(self$nodes)[sapply(self$nodes, function(x) x$isLeaf)])
    },

    nodeExists = function(path) path %in% names(self$nodes),

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

    setData = function(Sequence) {
      self$data <- Sequence
      for(sequence_vec in Sequence$data){
        private$fillData(sequence_vec)
      }
    },

    fillByDepth = function(depth) {
      for (i in seq_len(depth)) {
        leaves <- self$getLeaves()
        for(leaf in leaves){
          self$addChildren(leaf)
        }
      }
    },

    print = function() {
      print("Context Tree:")
      for (path in sort(self$getLeaves())) {
        print(self$nodes[[path]])
        cat("\n")
      }
    }
  ),
  private = list(
    addNode = function(path) {
      symbols <- str_split_1(path, "\\.")[-1]
      if (!self$nodeExists(path)) {
        node <- TreeNode$new(path)
        self$nodes[[path]] <- node
        return(node)
      }
      return(NULL)
    },

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
    }
  )
)
