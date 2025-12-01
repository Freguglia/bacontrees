#' @importFrom R6 R6Class
#' @title Bayesian Context Tree R6 Class
#'
#' @description
#' The `baConTree` class extends `ContextTree` to support Bayesian inference, including Dirichlet priors, context prior weights, and Metropolis-Hastings sampling for posterior inference on context trees.
#'
#' @param alpha Hyperparameter considered for the Dirichlet prior distribution
#' of probabilities.
#' @details
#' This class provides methods for setting priors, running MCMC, and extracting posterior samples for Bayesian context tree models.
#'
#' @examples
#' bt <- baConTree$new(abc_list, maximalDepth = 3, alpha = 0.01)
#' bt$setContextPriorWeights(function(node) -1/3*node$getDepth())
#' bt$runMetropolisHastings(300)
#' chain <- bt$getChain()
#'
#' @importFrom purrr walk map_dbl
#' @importFrom progressr progressor
#' @export
baConTree <- R6Class(
  "baConTree",
  inherit = ContextTree,
  public = list(

#' @param data Either a vector with discrete data or a lista of vectors.
#' @param maximalDepth Depth of the maximal tree considered.
#' @param active Either "root" or "maximal" to indicate which nodes
#' should be initialized as active.
#' @param priorWeights A function to be evaluated at each node that returns
#' its weight in the prior distribution.
    initialize = function(data, maximalDepth = 5, alpha = NULL, priorWeights = function(x) 0, active = "root") {
      super$initialize(data, maximalDepth, active)
      if(!self$validate()) {
        stop("Maximal Context Tree is invalid.")
      }
      if(!is.null(alpha)){
        self$setAlpha(alpha)
      }
      if(!is.null(alpha)){
        self$setContextPriorWeights(priorWeights)
      }
    },

#' @description
#' Sets the value of the Dirichlet priors if not initialized.
    setAlpha = function(alpha){
      if(private$hasAlpha){
        stop(paste0("alpha is already specified as ", private$alpha))
      }
      private$alpha <- alpha
      for(node in self$nodes) {
        node$extra$alpha <- rep(alpha, private$m)
      }
      private$computeIntegratedDirichlet()
      private$hasAlpha <- TRUE
    },

#' @param fn A function to be evaluated at each node that returns
#' its weight in the prior distribution.
    setContextPriorWeights = function(fn){
      for(node in self$nodes) {
        node$extra$priorWeight <- fn(node)
      }

      private$hasContextPrior <- TRUE
      if(private$hasAlpha){
        private$preComputeRatios()
      }
    },

#' @param steps Number of steps to run the Metropolis Hastings algorithm for.
#' @details
#' This method supports progress monitoring via the **progressr** package.
#' Users can wrap the function call in `with_progress()` to display a progress
#' bar while the function executes. If no progress handler is registered, the
#' function will run without showing progress.
#'
#' To enable progress, register a handler and wrap the function call in
#' `with_progress()`.
    runMetropolisHastings = function(steps){
      if(!(private$hasAlpha & private$hasContextPrior)){
        stop("Dirichlet alpha and context priors must be set prior to running the Metropolis Hastings algorithm.")
      }

      pb <- progressor(steps/10)
      if(is.null(private$chain)){
        private$chain <- data.frame(t = seq(0, steps, 1), tree = character(steps + 1))
        private$chain$tree[1] <- self$activeTreeCode()
      } else {
        private$chain <- rbind(private$chain, data.frame(t = seq(from = private$iterations + 1,
                                                                 private$iterations + steps, 1),
                                                         tree = character(steps)))
      }
      m <- length(private$Alphabet$symbols)
      for(t in seq_len(steps)){
        prune <- sample(0:1, 1)
        if(prune){
          if(length(private$prunableNodes) > 1){
            node_to_prune <- sample(private$prunableNodes, 1)
            pruning_leaf <- self$nodes[[node_to_prune]]$isLeaf()
            n_prunable <- length(private$prunableNodes)
            n_growable_after <- length(private$growableNodes) + 1 -(m*(!pruning_leaf))
            log_accept_ratio <-
              self$nodes[[node_to_prune]]$extra$prunePosteriorRatio +
              log(1/n_growable_after) -
              log(m/n_prunable)
            if(log(runif(1)) < log_accept_ratio)
              self$pruneActive(node_to_prune)
          }
        } else {
          if(length(private$growableNodes) > 0){
            node_to_grow <- sample(self$getGrowableNodes(), 1)
            n_growable <- length(private$growableNodes)
            growing_prunable <- node_to_grow %in% private$prunableNodes
            n_prunable_after <- length(private$prunableNodes) + m*(!growing_prunable)
            log_accept_ratio <-
              self$nodes[[node_to_grow]]$extra$growPosteriorRatio +
              log(m/n_prunable_after) -
              log(1/n_growable)
            if(log(runif(1)) < log_accept_ratio)
              self$growActive(node_to_grow)
          }
        }
        private$iterations <- private$iterations + 1
        private$chain$tree[private$iterations + 1] <- self$activeTreeCode()
        if(t%%10 == 0) pb()
      }
    },

#' @description Chain generated via Metropolis Hastings algorithm.
#' @returns Gets the sampled chain stored.
    getChain = function(){
      private$chain
    }
  ),
  private = list(
    hasAlpha = FALSE,
    alpha = numeric(0),
    hasContextPrior = FALSE,
    hasPrecomputedRatios = FALSE,
    iterations = 0,
    chain = NULL,
    computeIntegratedDirichlet = function(){
      for(node in self$nodes) {
        result <- lgamma(sum(node$extra$alpha)) -
          sum(lgamma(node$extra$alpha)) +
          sum(lgamma(node$extra$alpha + node$counts)) -
          lgamma(sum(node$extra$alpha + node$counts))
        node$extra$marginalNodeLL <- result
      }

      if(private$hasContextPrior){
        private$preComputeRatios()
      }
    },

    preComputeRatios = function(){
      for(node in self$nodes) {
        node$extra$posteriorWeight <- node$extra$priorWeight +
          node$extra$marginalNodeLL
      }

      for(node in self$nodes){
        if(!node$isLeaf()){
          childrenPosteriorSum <- sum(map_dbl(self$nodes[node$getChildrenPaths()],
                                              ~.x$extra$posteriorWeight))
          node$extra$childrenPosteriorWeight <- childrenPosteriorSum
        } else {
          node$extra$childrenPosteriorWeight <- NA
        }
        node$extra$growPosteriorRatio <- node$extra$childrenPosteriorWeight -
          node$extra$posteriorWeight
      }

      for(node in self$nodes){
        if(!node$getPath() == "*"){
          parent <- self$nodes[[node$getParentPath()]]
          node$extra$prunePosteriorRatio <- -parent$extra$growPosteriorRatio
        } else {
          node$extra$prunePosteriorRatio <- NA
        }
      }
    }
  )
)

#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom igraph V edge_attr<- ends E
#' @export
plot.baConTree = function(x, ...){
  ig <- x$igraph(activeOnly = FALSE)
  ends_mat <- ends(ig, E(ig))
  values <- V(ig)[ends_mat[, 2]]$prunePosteriorRatio
  edge_attr(ig, "prunePosteriorRatio") <- values
  ggraph(ig, layout = "tree") +
    geom_edge_diagonal0() +
    geom_node_label(aes(label = nodeLabel, fill = prunePosteriorRatio)) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = 0)
}
