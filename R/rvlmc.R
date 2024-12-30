#' @importFrom stringr str_count str_starts fixed
#' @export
rvlmc <- function(n, alphabet, context_list, context_probs){
  H <- max(str_count(context_list, "\\."))
  out <- sample(alphabet, size = n, replace = T)
  t <- H + 1
  while(t < n){
    Hpast <- paste0(c("*", out[t:(t-H)]), collapse = ".")
    contextIdx <- which(str_starts(Hpast, fixed(context_list)))
    out[t+1] <- sample(alphabet, size = 1, prob = context_probs[[contextIdx]])
    t <- t + 1
  }
  return(out)
}
