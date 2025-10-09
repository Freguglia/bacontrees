#' @title Generate a sequence based on a Variable Length Markov Chain (VLMC)
#'
#' @description This function simulates one or more sequences using a Variable Length Markov Chain (VLMC) model, where the memory (context) length can vary depending on the context tree provided.
#'
#' @param n An integer specifying the length of the sequence to generate, or a vector of lengths for multiple sequences.
#' @param alphabet A character vector representing the set of symbols used in the sequence.
#' @param context_list A character vector specifying the list of contexts in the VLMC. Each context should be a string where symbols are separated by dots (e.g., `"*.a"`, `"*.b"`, `"*.c.a"`).
#' @param context_probs A list of probability vectors corresponding to each context in `context_list`. Each vector specifies the probabilities of sampling each symbol in `alphabet` given the context.
#'
#' @return If `n` is a single integer, returns a character vector of length `n` representing the generated sequence. If `n` is a vector of length > 1, returns a list of character vectors, each of the specified length.
#'
#' @details
#' The function generates sequences by traversing the context tree defined by `context_list` and `context_probs`. For each position in the sequence, the most specific matching context is used to sample the next symbol according to the corresponding probability vector.
#'
#' @examples
#' # Define parameters for the VLMC
#' n <- 1000
#' alphabet <- c("a", "b", "c")
#' context_list <- c("*.a", "*.b", "*.c.a", "*.c.b", "*.c.c")
#' context_probs <- list(
#'   c(0.10, 0.20, 0.70), # for *.a
#'   c(0.33, 0.33, 0.34), # for *.b
#'   c(0.20, 0.10, 0.70), # for *.c.a
#'   c(0.01, 0.98, 0.01), # for *.c.b
#'   c(0.40, 0.40, 0.20)  # for *.c.c
#' )
#'
#' # Generate a single sequence
#' sequence <- rvlmc(n, alphabet, context_list, context_probs)
#' print(sequence)
#'
#' # Generate multiple sequences of different lengths
#' n_vec <- c(100, 200, 150)
#' sequences <- rvlmc(n_vec, alphabet, context_list, context_probs)
#' str(sequences)
#'
#' @importFrom stringr str_count str_starts fixed
#' @export
rvlmc <- function(n, alphabet, context_list, context_probs){
  if(length(n) == 1){
    rvlmc_single(n, alphabet, context_list, context_probs)
  } else if(length(n) > 1){
    lapply(n, function(x) rvlmc_single(x, alphabet, context_list, context_probs))
  } else {
    stop("'n' must have length at least 1.")
  }
}

rvlmc_single <- function(n, alphabet, context_list, context_probs){
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
