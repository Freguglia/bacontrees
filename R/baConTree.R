#' @importFrom purrr walk
#' @export
baConTree <- R6Class(
  "baConTree",
  inherit = ContextTree,
  public = list(

    initialize = function(data, maximal_depth = 5) {
      Alphabet <- Alphabet$new(sort(unique(unlist(data))))
      super$initialize(Alphabet)
      self$fillByDepth(maximal_depth)
      self$setData(Sequence$new(data))
      if(!self$validate()) {
        stop("Maximal Context Tree is invalid.")
      }
    },

    setAllDirichletPars = function(alpha){
      for(node in self$nodes) {
        node$extra$dirichletAlpha <- rep(alpha, self$m)
      }
    },

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
