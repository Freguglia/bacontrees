#' @importFrom R6 R6Class
Alphabet <- R6Class(
  "Alphabet",
  public = list(
    symbols = character(0),
    labels = character(0),
    length = 0,
    initialize = function(symbols) {
      idx <- seq_along(symbols)
      self$length <- length(symbols)
      self$symbols <- symbols
      self$labels <- symbols
      names(self$labels) <- seq_along(symbols)
    },
    print = function(...) {
      cat("Alphabet:\n")
      if (self$length <= 4) {
        for (i in seq_len(self$length)) {
          cat(paste0(i, " -> ", self$symbols[i], "\n"))
        }
      } else {
      }
    }
  ))
