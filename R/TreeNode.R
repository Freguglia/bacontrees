#' @importFrom stringr str_length str_sub
#' @export
TreeNode <- R6Class(
  "TreeNode",
  public = list(
    childrenIndex = character(0),
    counts = numeric(0),
    isLeaf = NA,
    extra = list(),
    initialize = function(path) {
      private$path <- path
      private$depth <- length(str_split_1(path, "\\.")) - 1
      if(private$depth > 0){
        self$parentIndex <- str_sub(self$path, end = -3)
      }
    },
    print = function(...) {
      cat(self$path)
    },
    activate = function() {private$active <- TRUE},
    deactivate = function() {private$active <- FALSE},
    isActive = function() {private$active},
    getDepth = function() {private$depth},
    getPath = function() {private$path},
    getParentPath = function() {private$parentPath}
    validatePath = function(Alphabet) {
      s <- str_split_1(private$path, "\\.")[-1]
      return(all(s %in% Alphabet$symbols))
    }
  ),
  private = list(
    path = character(0),
    parentPath = character(0),
    active = FALSE,
    depth = numeric(0)
  ))
