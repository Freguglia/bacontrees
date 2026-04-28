Alphabet <- R6Class(
  "Alphabet",
  public = list(
    symbols = character(0),
    length = 0,
    initialize = function(symbols) {
      self$length <- length(symbols)
      self$symbols <- symbols
    },
    print = function(...) {
      cat("Alphabet:\n")
      for (i in seq_len(self$length)) {
        cat(paste0(i, " -> ", self$symbols[i],
                   ifelse(i == self$length, "", ", ")))
      }
    }
  ))
