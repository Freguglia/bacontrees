#' @export
ContextTree <- R6Class(
  "ContextTree",
  public = list(
    nodes = list(),
    Alphabet = NULL,
    root = NULL,
    initialize = function(Alphabet = NULL) {
      self$Alphabet <- Alphabet
      root <- TreeNode$new(path = "*")
      self$nodes[[root$getPath()]] <- root
      self$root <- root
    },

    addNode = function(path) {
      symbols <- str_split_1(path, "\\.")[-1]
      if (any(!symbols %in% self$Alphabet$symbols)) {
        stop("Error: Path contains symbols not in the alphabet.")
      }
      if (!self$nodeExists(path)) {
        node <- TreeNode$new(path)
        self$nodes[[path]] <- node
        return(node)
      }
      return(NULL)
    },

    validate = function() {
      m <- length(self$Alphabet$symbols)
      for (path in names(self$nodes)) {
        children_paths <- names(self$nodes)[startsWith(names(self$nodes), paste0(path, "."))]
        children_depths <- sapply(children_paths, function(p) length(str_split_1(p, "\\.")))
        expected_depth <- length(str_split_1(path, "\\.")) + 1
        children_paths <- children_paths[children_depths == expected_depth]
        if (length(children_paths) != m) {
          return(FALSE)
        }
      }
      return(TRUE)
    },

    getNode = function(path) {
      if (self$nodeExists(path)) {
        return(self$nodes[[path]])
      }
      return(NULL)
    },

    nodeExists = function(path) { path %in% names(self$nodes) },

    print = function() {
      for (path in names(self$nodes)) {
        self$nodes[[path]]$print()
        cat("\n")
      }
    }
  )
)
