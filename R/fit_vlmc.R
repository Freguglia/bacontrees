#' @title Fit a Variable Length Markov Chain (VLMC) via Context Algorithm
#'
#' @description
#' Fits a VLMC model to discrete sequence data using the context algorithm, performing likelihood ratio tests to prune the context tree.
#'
#' @param data Either a character vector (single sequence) or a list of character vectors (multiple sequences).
#' @param cutoff Numeric. Cutoff value for the (log) likelihood ratio test statistic used for pruning.
#' @param max_length Integer. Depth of the maximal tree considered.
#'
#' @return A `ContextTree` object representing the fitted VLMC.
#'
#' @details
#' The function builds a maximal context tree, computes log-likelihoods for each node, and prunes nodes whose likelihood ratio test statistic is below the cutoff. The result is a pruned context tree representing the fitted VLMC.
#'
#' @examples
#' data(abc_list)
#' fit <- fit_vlmc(abc_list, cutoff = 20, max_length = 4)
#' print(fit$getActiveNodes())
#'
#' @export
fit_vlmc <- function(data, cutoff = 10, max_length = 6){
  ct <- ContextTree$new(data = data, maximalDepth = max_length, active = "maximal")

  # Set likelihood contribution of each node.
  for(node in ct$nodes){
    p <- node$counts/sum(node$counts)
    logLikelihood <- sum(node$counts*log(p))
    node$extra$logLikelihood <- ifelse(is.na(logLikelihood), 0, logLikelihood)
  }

  # Compute the sum of log-likelihood for the children of each node and
  # test statistics for the prunning of the children nodes.
  for(node in ct$nodes){
    children_paths <- node$getChildrenPaths()
    if(!node$isLeaf){
      children_nodes <- ct$getChildrenNodes(node$getPath(), idx = FALSE)
      children_likelihood_sum <-
        sum(map_dbl(children_nodes, ~.x$extra$logLikelihood))
      node$extra$childrenLogLikelihood <- children_likelihood_sum
      for(child in children_nodes){
        child$extra$pruneTestStatistic <-
          node$extra$childrenLogLikelihood - node$extra$logLikelihood
      }
    }
  }

  # Prune nodes while statistics are less than the cutoff value.
  prunable_nodes <- ct$getPrunableNodes()
  test_statistics <- map_dbl(ct$nodes[prunable_nodes], ~.x$extra$pruneTestStatistic)
  while(any(test_statistics < cutoff)){
    to_prune <- which(test_statistics < cutoff)[1]
    ct$pruneActive(prunable_nodes[to_prune])
    prunable_nodes <- ct$getPrunableNodes()
    test_statistics <- map_dbl(ct$nodes[prunable_nodes], ~.x$extra$pruneTestStatistic)
  }

  return(ct)
}
