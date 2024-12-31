#' @title Generate a sequence based on a Variable Length Markov Chain (VLMC)
#'
#' @description This function simulates a sequence using a Variable Length Markov Chain (VLMC) model.
#'
#' @param n An integer specifying the length of the sequence to generate.
#' @param alphabet A character vector representing the set of symbols used in the sequence.
#' @param context_list A character vector specifying the list of contexts in the VLMC. Each context should be a string where symbols are separated by dots (e.g., `"a.b.c"`).
#' @param context_probs A list of probability vectors corresponding to each context in `context_list`. Each vector specifies the probabilities of sampling each symbol in `alphabet` given the context.
#'
#' @return A character vector of length `n` representing the generated sequence.
#'
#' @examples
#' # Define parameters for the VLMC
#' n <- 1000
#' alphabet <- c("a", "b", "c")
#' context_list <- c("*.a", "*.b", "*.c.a", "*.c.b", "*.c.c")
#' context_probs <- list(
#'   c(0.10, 0.20, 0.70), #.a
#'   c(0.33, 0.33, 0.34), #.b
#'   c(0.20, 0.10, 0.70), #.c.a
#'   c(0.01, 0.98, 0.01), #.c.b
#'   c(0.40, 0.40, 0.20)  #.c.c
#' )
#'
#' # Generate a sequence
#' sequence <- rvlmc(n, alphabet, context_list, context_probs)
#' print(sequence)
#'
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
