#' @title Bayesian Context Tree R6 class
#'
#' @importFrom purrr walk map_dbl
#' @importFrom progress progress_bar
#' @export
baConTree <- R6Class(
  "baConTree",
  inherit = ContextTree,
  public = list(

    #' @param data Either a vector with discrete data or a lista of vectors.
    #' @param maximalDepth Depth of the maximal tree considered.
    #' @param active Either "root" or "maximal" to indicate which nodes
    #' should be initialized as active.
    initialize = function(data, maximalDepth = 5, active = "root") {
      Alphabet <- Alphabet$new(sort(unique(unlist(data))))
      super$initialize(Alphabet, maximalDepth, active)
      self$setData(Sequence$new(data, Alphabet))
      if(!self$validate()) {
        stop("Maximal Context Tree is invalid.")
      }
    },

    #' @param alpha Hyperparameter considered for the Dirichlet prior distribution
    #' of probabilities.
    setAllDirichletPars = function(alpha){
      for(node in self$nodes) {
        node$extra$dirichletAlpha <- rep(alpha, private$m)
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
    runMetropolisHastings = function(steps){
      if(!(private$hasAlpha & private$hasContextPrior)){
        stop("Dirichlet alpha and context priors must be set prior to running the Metropolis Hastings algorithm.")
      }

      pb <- progress_bar$new(total = steps,
                             format = "Running Metropolis step :current/:total [:bar] :percent | rate: :tick_rate/s | eta: :eta")
      chain <- data.frame(t = seq(0, steps, 1), tree = character(steps + 1))
      chain$tree[1] <- self$activeTreeCode()
      m <- length(private$Alphabet$symbols)
      for(t in seq_len(steps)){
        prune <- sample(0:1, 1)
        if(prune){
          if(length(private$prunableNodes) > 1){
            node_to_prune <- sample(private$prunableNodes, 1)
            pruning_leaf <- self$nodes[[node_to_prune]]$isLeaf
            n_prunable <- length(private$prunableNodes)
            n_growable_after <- length(private$growableNodes) + 1 -(m*pruning_leaf)
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
        chain$tree[t+1] <- self$activeTreeCode()
        pb$tick()
      }
      return(chain)
    }
  ),
  private = list(
    hasAlpha = FALSE,
    hasContextPrior = FALSE,
    hasPrecomputedRatios = FALSE,
    computeIntegratedDirichlet = function(){
      for(node in self$nodes) {
        result <- lgamma(sum(node$extra$dirichletAlpha)) -
          sum(lgamma(node$extra$dirichletAlpha)) +
          sum(lgamma(node$extra$dirichletAlpha + node$counts)) -
          lgamma(sum(node$extra$dirichletAlpha + node$counts))
        node$extra$integratedDirichletLog <- result
      }

      if(private$hasContextPrior){
        private$preComputeRatios()
      }
    },

    preComputeRatios = function(){
      for(node in self$nodes) {
        node$extra$posteriorWeight <- node$extra$priorWeight +
          node$extra$integratedDirichletLog
      }

      for(node in self$nodes){
        if(!node$isLeaf){
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
