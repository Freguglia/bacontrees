#' @title Create ContextTree from Context Vector
#'
#' @description
#' Constructs a `ContextTree` object from a vector or string of context paths, initializing the tree to match the specified contexts.
#'
#' @param contexts Character vector or string. Contexts separated by commas, or a vector of context strings (e.g., "*.a", "*.b", ...).
#'
#' @return A `ContextTree` object with maximal length equal to the largest depth among specified contexts and active tree equal to the specified contexts.
#'
#' @examples
#' treeFromContexts(c("*.0", "*.1.0", "*.1.1"))
#' treeFromContexts("{*.a, *.b, *.c.a, *.c.b, *.c.c}")
#'
#' @importFrom stringr str_split str_replace_all str_count
#' @export
treeFromContexts <- function(contexts){
  if(length(contexts) == 1){
    raw <- str_replace_all(contexts, "\\{|\\}", "")
    parts <- str_split(string = raw, pattern = ",")[[1]]
    contexts <- trimws(parts)
    contexts <- contexts[ nzchar(contexts) ]
  }

  if(!validate_tree_string(contexts)){
    stop("Contexts do no correspond to a full tree.")
  }

  max_depth <- max(str_count(contexts, "\\."))
  alphabet <- unique(unlist(str_split(contexts, "\\.")))
  alphabet <- setdiff(alphabet, "*")

  ct <- ContextTree$new(alphabet = alphabet, maximalDepth = max_depth, active = "root")

  current_depth <- 0
  while(length(setdiff(ct$getActiveNodes(idx = TRUE), contexts)) > 0){
    subcontexts <- substr(contexts, start = 1, stop = current_depth*2 + 1)
    for(sc in unique(subcontexts)){
      if(!sc %in% contexts){
        ct$growActive(sc)
      }
    }
    current_depth <- current_depth + 1
  }

  ct
}
