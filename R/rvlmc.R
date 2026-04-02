#' @title Generate a sequence based on a Variable Length Markov Chain (VLMC)
#'
#' @description This function simulates one or more sequences using a Variable Length Markov Chain (VLMC) model, where the memory (context) length can vary depending on the context tree provided.
#'
#' @param n An integer specifying the length of the sequence to generate, or a vector of lengths for multiple sequences.
#' @param alphabet A character vector representing the set of symbols used in the sequence.
#' @param context_list A character vector specifying the contexts of the VLMC.
#'   Each context is a dot-separated string starting with `"*"` (the root
#'   sentinel), where subsequent symbols represent the observed past in
#'   **reverse chronological order** (most recent first). For example,
#'   `"*.c.b"` denotes the context where the last observed symbol was `"c"`
#'   and the one before it was `"b"`. The set of contexts must form a
#'   **complete partition** of all possible pasts (i.e., a valid full context
#'   tree); the function will error if this is not satisfied.
#' @param context_probs A list of numeric probability vectors, one per element
#'   of `context_list`, in the same order. Each vector must have the same
#'   length as `alphabet`, with elements giving the probability of sampling
#'   the corresponding symbol in `alphabet`.
#'
#' @return If `n` is a single integer, returns a character vector of length `n`
#'   representing the generated sequence. If `n` is a vector of length > 1,
#'   returns a list of character vectors, each of the specified length.
#'
#' @details
#' The function generates sequences by traversing the context tree defined by
#' `context_list` and `context_probs`. For each position in the sequence, the
#' most specific matching context in `context_list` is used to sample the next
#' symbol according to the corresponding probability vector.
#'
#' The first `H` symbols (where `H` is the depth of the deepest context) are
#' sampled uniformly from `alphabet` as an initialisation step and are not
#' drawn from the VLMC model.
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
  derived_alphabet <- setdiff(unique(unlist(strsplit(context_list, "\\."))), "*")
  if(!setequal(sort(derived_alphabet), sort(alphabet))){
    stop("'context_list' is incompatible with a full tree for the provided 'alphabet'.")
  }
  if(!validate_tree_string(context_list)){
    stop("'context_list' is incompatible with a full tree.")
  }
  if(length(context_list) != length(context_probs)){
    stop("List of probability vectors incompatible with the 'context_tree' specified.")

  }
  if(any(map_int(context_probs, length) != length(alphabet))){
    stop("Probabilities vectors must have length compatible with the alphabet.")
  }
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
