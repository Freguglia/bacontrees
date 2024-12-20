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
    },

    #' @param fn A function to be evaluated at each node that returns
    #' its weight in the prior distribution.
    setContextPriorWeights = function(fn){
      for(node in self$nodes) {
        node$extra$priorWeight <- fn(node)
      }

      for(node in self$nodes) {
        node$extra$childrenPriorWeight <- 0
        for(child in self$nodes[node$childrenIndex]){
          node$extra$childrenPriorWeight <- node$extra$childrenPriorWeight +
            child$extra$priorWeight
        }
      }
    }
  ),
  private = list(
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
          childrenIntegratedSum <- sum(map_dbl(self$nodes[node$childrenIndex],
                                               ~.x$extra$integratedDirichletLog))
          node$extra$childrenIntegratedDirichletLog <- childrenIntegratedSum
        } else {
          node$extra$childrenIntegratedDirichletLog <- NA
        }
        node$extra$logIntegratedRatio <- node$extra$integratedDirichletLog -
          node$extra$childrenIntegratedDirichletLog
      }
    }
  )
)
