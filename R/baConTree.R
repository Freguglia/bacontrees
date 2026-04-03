#' @importFrom R6 R6Class
#' @title Bayesian Context Tree R6 Class
#'
#' @description
#' The `baConTree` class extends `ContextTree` to support Bayesian inference, including Dirichlet priors, context prior weights, and Metropolis-Hastings sampling for posterior inference on context trees.
#'
#' @param alpha Hyperparameter considered for the Dirichlet prior distribution
#' of probabilities.
#' @details
#' This class provides methods for running MCMC and extracting posterior samples for Bayesian context tree models.
#'
#' @examples
#' bt <- baConTree$new(abc_list, maximalDepth = 3, alpha = 0.01,
#'                     priorWeights = function(node) exp(-1/3*node$getDepth()))
#' bt$runMetropolisHastings(300)
#' chain <- bt$getChain()
#'
#' @importFrom purrr walk map_dbl
#' @importFrom progressr progressor
#' @importFrom Brobdingnag as.brob cbrob
#' @export
baConTree <- R6Class(
  "baConTree",
  inherit = ContextTree,
  public = list(

    #' @param data Either a vector with discrete data or a list of vectors.
    #' @param maximalDepth Depth of the maximal tree considered.
    #' @param alpha Hyperparameter for the Dirichlet prior distribution of probabilities.
    #' @param priorWeights A function to be evaluated at each node that returns
    #' its weight in the prior distribution.
    initialize = function(data, maximalDepth = 5, alpha, priorWeights) {
      super$initialize(data, maximalDepth)
      self$activateRoot()
      if(!self$validate()) {
        stop("Maximal Context Tree is invalid.")
      }
      private$alpha <- alpha
      for(node in self$nodes) {
        node$extra$alpha <- rep(alpha, private$m)
      }
      for(node in self$nodes) {
        node$extra$priorWeight <- as.brob(priorWeights(node))
        node$extra$logPriorWeight <- log(node$extra$priorWeight)
      }
      private$computeLogQ()
      private$preComputeRatios()
      private$buildRecursiveSigma()
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
    },

    #' @description Marginal likelihood of the data under the Bayesian context tree model.
    #' @param log Logical. If `TRUE` (default), returns the log marginal likelihood
    #' as a plain R `numeric`. If `FALSE`, returns the marginal likelihood as a
    #' `brob` object.
    #' @details
    #' This is computed from `sigmaPosterior/sigmaPrior` at the root node,
    #' which equals the sum over all trees of `prior(T) * p(data | T)`,
    #' normalised by the prior partition function.
    #' @returns A `numeric` (when `log = TRUE`) or a `brob` (when `log = FALSE`).
    getMarginalLikelihood = function(log = TRUE){
      root <- self$nodes[["*"]]
      if(log){
        as.numeric(log(root$extra$sigmaPosterior) - log(root$extra$sigmaPrior))
      } else {
        root$extra$sigmaPosterior / root$extra$sigmaPrior
      }
    }
  ),
  private = list(
    alpha = numeric(0),
    iterations = 0,
    chain = NULL,
    computeLogQ = function(){
      for(node in self$nodes) {
        result <- lgamma(sum(node$extra$alpha)) -
          sum(lgamma(node$extra$alpha)) +
          sum(lgamma(node$extra$alpha + node$counts)) -
          lgamma(sum(node$extra$alpha + node$counts))
        node$extra$nodeLogQ <- result
        node$extra$logPosteriorWeight <- node$extra$logPriorWeight +
          node$extra$nodeLogQ
        node$extra$PosteriorWeight <- exp(as.brob(node$extra$logPosteriorWeight))
      }
    },


    preComputeRatios = function(){
      for(node in self$nodes){
        if(!node$isLeaf()){
          childrenPosteriorSum <- sum(map_dbl(self$nodes[node$getChildrenPaths()],
                                              ~.x$extra$logPosteriorWeight))
          node$extra$childrenPosteriorWeight <- childrenPosteriorSum
        } else {
          node$extra$childrenPosteriorWeight <- NA
        }
        node$extra$growPosteriorRatio <- node$extra$childrenPosteriorWeight -
          node$extra$logPosteriorWeight
      }

      for(node in self$nodes){
        if(!node$getPath() == "*"){
          parent <- self$nodes[[node$getParentPath()]]
          node$extra$prunePosteriorRatio <- -parent$extra$growPosteriorRatio
        } else {
          node$extra$prunePosteriorRatio <- NA
        }
      }
    },

    buildRecursiveSigma = function(){
      L <- self$getMaximalDepth()
      nodes <- self$nodes[sapply(self$nodes, function(node) node$getDepth()) == L]
      for(node in nodes){
        node$extra$sigmaPrior <- as.brob(node$extra$priorWeight)
        node$extra$sigmaPosterior <- node$extra$PosteriorWeight
      }
      L <- L - 1
      while(L >= 0){
        nodes <- self$nodes[sapply(self$nodes, function(node) node$getDepth()) == L]
        for(node in nodes){
          node_children <- self$nodes[node$getChildrenPaths()]
          children_priorSigmas <- cbrobl(sapply(node_children, function(node) node$extra$sigmaPrior))
          children_posteriorSigmas <- cbrobl(sapply(node_children, function(node) node$extra$sigmaPosterior))
          node$extra$sigmaPrior <- node$extra$priorWeight + prod(children_priorSigmas)
          node$extra$sigmaPosterior <- node$extra$priorWeight + prod(children_posteriorSigmas)
        }
        L <- L - 1
      }
    }
  )
)

#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom igraph V edge_attr<- ends E
#' @export
plot.baConTree <- function(x, ...){
  ig <- x$igraph(activeOnly = FALSE)
  ends_mat <- ends(ig, E(ig))
  values <- V(ig)[ends_mat[, 2]]$prunePosteriorRatio
  edge_attr(ig, "prunePosteriorRatio") <- values
  ggraph(ig, layout = "tree") +
    geom_edge_diagonal0() +
    geom_node_label(aes(label = nodeLabel, fill = prunePosteriorRatio)) +
    scale_fill_gradient2(low = "red", high = "blue", midpoint = 0)
}

cbrobl <- function(x){
  do.call(cbrob, unname(x))
}
