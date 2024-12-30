#' @importFrom stringr str_count
#' @export
rvlmc <- function(n, alphabet, context_list, context_probs){
  H <- max(str_count(context_list, "\\."))
  t <- 0
  return(H)
}
