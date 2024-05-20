#' @importFrom glue glue
#' @export
Sequence <- R6Class(
  "Sequence",
  public = list(
    data = character(0),
    n = 0,
    Alphabet = NULL,
    initialize = function(x) {
      self$Alphabet <- Alphabet$new(sort(unique(x)))
      self$n <- length(x)
      self$data <- x
    },
    print = function(...) {
      cat("Alphabet:", self$Alphabet$symbols, "\n")
      cat(glue("Sequence (n = {self$n}):"), "\n")
      if (self$n < 15){
        cat(self$data)
      } else {
        cat(self$data[1:14], "...")
      }
    }
  ))
