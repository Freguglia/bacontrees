#' @importFrom stringr str_split str_replace_all str_count
#'
#' @title Create ContextTree object from a vector of contexts
#'
#' @param contexts Either a string with contexts separated by `','` or
#' a vector of strings representing contexts.
#'
#' @examples
#' treeFromContexts(c("*.0", "*.1.0", "*.1.1"))
#' treeFromContexts("{*.a, *.b, *.c.a, *.c.b, *.c.c}")
#'
#'
#' @return A `ContextTree` object with maximal length equal to the largest
#' depth among specified contexts and active tree equal to the Context Tree
#' specified by the contexts.
#' @export
treeFromContexts <- function(contexts){
  if(length(contexts) == 1){
    contexts <- str_replace_all(contexts, "\\{|\\}| ", "")
    contexts <- str_split(string = contexts, pattern = ",")[[1]]
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
