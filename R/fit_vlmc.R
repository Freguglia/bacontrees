#' @title Fit a VLMC model via Context Algorithm
#'
#' @param data Either a vector with discrete data or a lista of vectors.
#' @param max_length Depth of the maximal tree considered.
#' @param cutoff Cutoff value for the (log) likelihood ratio test statistic.
#'
#' @export
fit_vlmc <- function(data, cutoff = 10, max_length = 6){
  alphabet <- Alphabet$new(sort(unique(unlist(data))))
  ct <- ContextTree$new(alphabet, maximalDepth = max_length, active = "maximal")
  ct$setData(data)

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
