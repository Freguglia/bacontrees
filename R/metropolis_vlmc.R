#' @title Metropolis-Hastings Posterior Sampling for Context Trees
#'
#' @description
#' Runs the Metropolis-Hastings algorithm to sample from the posterior distribution of context trees given sequence data, Dirichlet priors, and context prior weights.
#'
#' @param data Either a character vector (single sequence) or a list of character vectors (multiple sequences).
#' @param n_steps Integer. Number of MCMC steps to run.
#' @param max_depth Integer. Maximum depth for the context tree.
#' @param alpha Numeric. Dirichlet prior parameter for transition probabilities.
#' @param context_weights Function. Returns the log prior weight for a given node.
#' @param burnin Integer. Number of initial iterations to discard from posterior summaries.
#' @param thin Integer. Thinning interval for posterior summaries.
#' @param initial_root Logical. If TRUE, start with root-only tree; if FALSE, start with maximal tree.
#'
#' @details
#' This function supports progress monitoring via the **progressr** package. Wrap the call in `with_progress()` to display a progress bar.
#'
#' @return An object of class `metropolis_vlmc` with elements:
#'   - `df`: Data frame of context sets, tree codes, counts, and posterior probabilities.
#'   - `codes`: List of context sets for each tree code.
#'   - `chain`: The full chain of tree codes sampled.
#'
#' @examples
#' data(abc_list)
#' result <- metropolis_vlmc(abc_list, n_steps = 1000, max_depth = 3)
#' print(result)
#'
#' @importFrom dplyr group_by summarise mutate filter arrange rename n select %>% desc
#' @export
metropolis_vlmc <- function(data, n_steps, max_depth = 6,
                            alpha = 0.001,
                            context_weights = function(node) 0,
                            burnin = 100,
                            thin = 1,
                            initial_root = TRUE){
  if(initial_root){
    init <- "root"
  } else {
    init <- "maximal"
  }
  bt <- baConTree$new(data, maximalDepth = max_depth, active = init)
  bt$setAllDirichletPars(alpha)
  bt$setContextPriorWeights(context_weights)

  bt$runMetropolisHastings(n_steps)

  chain <- bt$getChain()$tree
  distribution <- bt$getChain() %>%
    filter(t > burnin, t %% thin == 0) %>%
    group_by(tree) %>%
    summarise(n = n()) %>%
    mutate(prob = n/sum(n)) %>%
    arrange(desc(prob)) %>%
    rename(tree_code = tree)

  codes <- lapply(distribution$tree_code, function(treecode) {
    bt$activateByCode(treecode)
    bt$getActiveNodes()
  })
  names(codes) <- distribution$tree_code

  contexts <- sapply(codes, function(x) glue("{{{paste0(x, collapse = ',')}}}"))
  distribution$tree_contexts <- contexts
  distribution <- distribution %>%
    select(tree_contexts, prob, n, tree_code)

  out <- list(df = distribution, codes = codes, chain = chain)
  class(out) <- "metropolis_vlmc"

  return(out)
}

#' @export
print.metropolis_vlmc <- function(x, ...){
  cat("Tree with highest posterior probability (",glue("{format(x$df$prob[1], digits = 3)}"), "):\n")
  cat("{", glue("{paste0(x$codes[[1]], collapse = ', ')}"), "}\n")
  print(x$df, n = 4)
}
