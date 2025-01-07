#' @title Bayesian Context Tree R6 class
#'
#' @importFrom purrr walk map_dbl
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
      self$setData(Sequence$new(data))
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

      for(node in self$nodes) {
        node$extra$childrenPriorWeight <- 0
        for(child in self$nodes[node$getChildrenPaths()]){
          node$extra$childrenPriorWeight <- node$extra$childrenPriorWeight +
            child$extra$priorWeight
        }
      }
      private$hasContextPrior <- TRUE
    },

    #' @param steps Number of steps to run the Metropolis Hastings algorithm for.
    runMetropolisHastings = function(steps){
      if(!(private$hasAlpha & private$hasContextPrior)){
        stop("Dirichlet alpha and context priors must be set prior to running the Metropolis Hastings algorithm.")
      }
      if(!private$hasPrecomputedRatios){
        private$preComputeRatios()
      }
      chain <- data.frame(t = seq(0, steps, 1), tree = character(steps + 1))
      chain$tree[1] <- self$activeTreeCode()
      for(t in seq_len(steps)){
        prune <- sample(0:1, 1)
        if(prune){
          if(length(private$prunableNodes) > 1){
            node_to_prune <- sample(self$getPrunableNodes(), 1)
            if(log(runif(1)) < self$nodes[[node_to_prune]]$extra$pruneLogPosteriorRatio)
            self$pruneActive(node_to_prune)
          }
        } else {
          if(length(private$growableNodes) > 0){
            node_to_grow <- sample(self$getGrowableNodes(), 1)
            if(log(runif(1)) < self$nodes[[node_to_grow]]$extra$growLogPosteriorRatio)
            self$growActive(node_to_grow)
          }
        }
        chain$tree[t+1] <- self$activeTreeCode()
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

      for(node in self$nodes) {
        if(!node$isLeaf){
          childrenIntegratedSum <- sum(map_dbl(self$nodes[node$getChildrenPaths()],
                                               ~.x$extra$integratedDirichletLog))
          node$extra$childrenIntegratedDirichletLog <- childrenIntegratedSum
        } else {
          node$extra$childrenIntegratedDirichletLog <- NA
        }
        node$extra$growLogPosteriorRatio <-
          node$extra$childrenIntegratedDirichletLog -
          node$extra$integratedDirichletLog
      }

    },

    preComputeRatios = function(){
      for(node in self$nodes) {
        if(!node$getPath() == "*"){
          parent <- self$nodes[[node$getParentPath()]]
          node$extra$pruneLogPosteriorRatio <- -parent$extra$growLogPosteriorRatio
        } else {
          node$extra$pruneLogPosteriorRatio <- NA
        }
      }
    }
  )
)
