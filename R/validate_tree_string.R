validate_tree_string <- function(contexts) {
  if (any(duplicated(contexts))) {
    return(FALSE)
  }

  toks_list <- strsplit(contexts, "\\.")
  alphabet <- setdiff(unique(unlist(toks_list)), "*")

  contexts2 <- gsub("([][{}()+*^${|\\\\?.])", "\\\\\\1", contexts)
  to_validate <- c("\\*")
  validated <- c()

  while(length(to_validate) > 0){
    node <- to_validate[1]
    if(node %in% contexts2){
      validated <- c(validated, node)
      to_validate <- to_validate[-1]
    } else {
      children <- paste0(node, "\\.", alphabet)
      in_tree <- sapply(children, function(x) any(grepl(x = contexts, pattern = x)))
      if(any(!in_tree)){
        return(FALSE)
      }
      to_validate <- c(to_validate[-1], children)
    }
  }
  if(setequal(validated, contexts2)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
