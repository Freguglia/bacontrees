#' @title Metropolis-Hastings for Context Trees
#'
#' @description
#' Generates a Markov Chain of Context Trees using the Metropolis-Hastings
#' algorithm given some sequence data set.
#'
#' @param data Either a vector with discrete data or a lista of vectors.
#' @param n_steps Number of steps to run the Metropolis-Hastings algorithm for.
#' @param max_depth Maximum depth considered for the context tree (using high
#' values may demand too much memory).
#' @param alpha Hyper parameters for the Dirichlet prior distribution used
#' for transition probabilities.
#' @param context_weights A function of a TreeNode that returns the contribution
#' of a given context in the tree prior distribution (in log scale).
#' See \link[bacontrees]{TreeNode} for details.
#' @param burnin Number of initial iterations to exclude from posterior
#' summarization results.
#' @param thin Number of iterations between observations used in the
#' posterior distribution summarization.
#' @param initial_root Logical value indicating whether the initial tree
#' should contain only the root node. If set to `FALSE`, the maximal tree
#' of the specified depth is used.
#'
#' @details
#' This function supports progress monitoring via the **progressr** package.
#' Users can wrap the function call in `with_progress()` to display a progress
#' bar while the function executes. If no progress handler is registered, the
#' function will run without showing progress.
#'
#' To enable progress, register a handler and wrap the function call in
#' `with_progress()`.
#'
#' @return A `metropolis_vlmc` object containing attributes:
#'
#'  * `df`: A table containing the set of contexts, tree code, counts and posterior probabilities
#'  considering the specified burnin and thin values.
#'  * `codes`: A list containing the contexts for the trees corresponding to each tree code.
#'  * `chain`: The complete chain generated, represented by tree codes.
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
