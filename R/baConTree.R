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
      private$buildRecursiveFunctions()
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
      self$activateRoot()
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

    #' @description Samples a context tree exactly from the prior or posterior
    #' distribution using the per-node branching probabilities.
    #' @param type Either `"prior"` or `"posterior"`. Determines which branching
    #' probabilities are used for sampling. `"prior"` uses
    #' `priorBranchingProbability` and `"posterior"` uses
    #' `posteriorBranchingProbability`.
    #' @details
    #' The algorithm starts from the root-only tree and at each non-leaf node
    #' the tree is grown (via `growActive`) with probability equal to the node's
    #' branching probability; otherwise the node remains a leaf of the sampled
    #' tree and its subtree is never visited.
    #' This yields an exact sample from the prior or posterior
    #' distribution over context trees.
    #'
    #' The branching probability at a node is
    #' `prod(children sigmaPrior) / sigmaPrior` for the prior, and the
    #' analogous quantity for the posterior, so that it equals one minus the
    #' probability that the process stops at that node.
    #' @return Invisibly returns the active tree code of the sampled tree (a
    #' character string as produced by `activeTreeCode()`). As a side-effect,
    #' the object's active tree is set to the sampled tree.
    sampleTree = function(type = c("prior", "posterior")) {
      type <- match.arg(type)
      prob_field <- if (type == "prior") "priorBranchingProbability" else "posteriorBranchingProbability"

      self$activateRoot()
      queue <- "*"

      while (length(queue) > 0) {
        path <- queue[[1]]
        queue <- queue[-1]
        node <- self$nodes[[path]]

        if (node$isLeaf()) next

        branch_prob <- node$extra[[prob_field]]
        if (runif(1) < branch_prob) {
          self$growActive(path)
          queue <- c(queue, node$getChildrenPaths())
        }
      }

      invisible(self$activeTreeCode())
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
    },

    #' @description
    #' Activates the smallest Maximum a Posteriori (MAP) tree under the Bayesian
    #' context tree model.
    #' @details
    #' Starting from the root node (initially active) and proceeding recursively
    #' through its descendants. The method compares the posterior weight at the node
    #' with the maximum posterior value attainable over all sub-tree configurations below it.
    #' If the posterior weight at the node attains the maximum (`node$extra$max = TRUE`) the node
    #' remains active and the recursion stops along that branch (i.e., the sub-tree below the
    #' node is pruned). Otherwise, the node is deactivated, its children are activated, and the
    #' procedure continues recursively.
    #' @returns
    #' Invisibly returns the contexts of the MAP tree and makes the MAP tree the
    #' active tree of the object.

    activateMap = function(){
      self$activateRoot()
      repeat {
        activenodes <- self$getActiveNodes(FALSE)
        changed <- FALSE
        for (node in activenodes) {
          if (node$extra$max == FALSE) {
            childs <- node$getChildrenPaths()
            for (path in childs) {
              self$nodes[[path]]$activate()
            }
            node$deactivate()
            changed <- TRUE
          }
        }
        if (!changed) break
      }
      invisible(self$getActiveNodes())},

    #' @description
    #' Computes the prior and posterior probabilities of the active tree under the
    #' Bayesian context tree model.
    #' @details
    #' The probabilities are obtained by taking the product of the weights associated
    #' with all active nodes in the tree. These quantities are then normalized by the
    #' corresponding normalizing constants, namely the value of `sigmaPrior` at the
    #' root node for the prior and the value of `sigmaPosterior` at the root for the posterior.
    #'
    #' For numerical stability, all computations are performed on the logarithmic scale,
    #' which avoids underflow when dealing with small values.
    #' @param log Logical. If `FALSE` (default), returns the prior and posterior
    #' probabilities of the active tree. If `TRUE`, returns their logarithms.
    #' @returns
    #' A list of length two, containing the prior and posterior probabilities
    #' (or their logarithms, if `log = TRUE`), in this order.
    infoActiveTree = function(log = FALSE){
      root <- self$nodes[["*"]]
      log_prior <- 0
      log_posterior <- 0
      for (node in self$getActiveNodes(FALSE)) {
        log_prior <- log_prior + node$extra$logPriorWeight
        log_posterior <- log_posterior + node$extra$logPosteriorWeight
      }
      log_sigmaPrior <- log(root$extra$sigmaPrior)
      log_sigmaPosterior <- log(root$extra$sigmaPosterior)

      if(log){
        list(log_prior = as.numeric(log_prior - log_sigmaPrior),
             log_posterior = as.numeric(log_posterior - log_sigmaPosterior))
      } else {
        list(prior =  exp(log_prior - log_sigmaPrior),
             posterior = exp(log_posterior - log_sigmaPosterior))
      }}),

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

    buildRecursiveFunctions = function(){
      L <- self$getMaximalDepth()
      nodes <- self$nodes[sapply(self$nodes, function(node) node$getDepth()) == L]
      for(node in nodes){
        node$extra$sigmaPrior <- as.brob(node$extra$priorWeight)
        node$extra$sigmaPosterior <- node$extra$PosteriorWeight
        node$extra$maxPosterior <- node$extra$PosteriorWeight
        node$extra$max <- TRUE
        node$extra$priorBranchingProbability <- 0
        node$extra$posteriorBranchingProbability <- 0
      }
      L <- L - 1
      while(L >= 0){
        nodes <- self$nodes[sapply(self$nodes, function(node) node$getDepth()) == L]
        for(node in nodes){
          node_children <- self$nodes[node$getChildrenPaths()]
          children_priorSigmas <- cbrobl(sapply(node_children, function(node) node$extra$sigmaPrior))
          children_posteriorSigmas <- cbrobl(sapply(node_children, function(node) node$extra$sigmaPosterior))
          children_posteriorMaxs <- cbrobl(sapply(node_children, function(node) node$extra$maxPosterior))
          node$extra$sigmaPrior <- node$extra$priorWeight + prod(children_priorSigmas)
          node$extra$sigmaPosterior <- node$extra$PosteriorWeight + prod(children_posteriorSigmas)
          if (node$extra$PosteriorWeight >= prod(children_posteriorMaxs)) {
            node$extra$maxPosterior <- node$extra$PosteriorWeight
            node$extra$max <- TRUE
          } else {
            node$extra$maxPosterior <- prod(children_posteriorMaxs)
            node$extra$max <- FALSE}
          node$extra$priorBranchingProbability <- as.numeric(prod(children_priorSigmas) / node$extra$sigmaPrior)
          node$extra$posteriorBranchingProbability <- as.numeric(prod(children_posteriorSigmas) / node$extra$sigmaPosterior)
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
