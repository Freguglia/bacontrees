#' @importFrom purrr walk map_dbl
#' @export
baConTree <- R6Class(
  "baConTree",
  inherit = ContextTree,
  public = list(

    initialize = function(data, maximal_depth = 5, active = "root") {
      Alphabet <- Alphabet$new(sort(unique(unlist(data))))
      super$initialize(Alphabet)
      self$fillByDepth(maximal_depth)
      self$setData(Sequence$new(data))
      if(!self$validate()) {
        stop("Maximal Context Tree is invalid.")
      }

      if(active == "root"){
        self$activateRoot()
      } else {
        self$activateMaximal()
      }
    },

    setAllDirichletPars = function(alpha){
      for(node in self$nodes) {
        node$extra$dirichletAlpha <- rep(alpha, self$m)
      }
      private$computeIntegratedDirichlet()
    },

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
    },
    growActive = function(nodePath){
      node <- self$nodes[[nodePath]]
      if(node$isActive() & !node$isLeaf){
        node$deactivate()
        for(child in self$nodes[node$childrenIndex]){
          child$activate()
        }
      } else {
        stop("Cannot grow a node that is not active or is a leaf node.")
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
