#' @importFrom glue glue
#' @export
Sequence <- R6Class(
  "Sequence",
  public = list(
    data = character(0),
    n = 0,
    Alphabet = NULL,
    initialize = function(x) {
      self$Alphabet <- Alphabet$new(sort(unique(unlist(x))))
      if(is.list(x)){
        self$n <- sapply(x, length)
        self$data <- x
      } else if(is.character(x)){
        self$n <- length(x)
        self$data <- list(x)
      } else {
        stop("'x' must be either a list or a character vector.")
      }
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
