#' @importFrom glue glue
Sequence <- R6Class(
  "Sequence",
  public = list(
    data = character(0),
    n = 0,
    Alphabet = NULL,
    initialize = function(x, alphabet = NULL) {
      if(is.null(alphabet)){
        self$Alphabet <- Alphabet$new(unique(unlist(x)))
      } else {
        self$Alphabet <- alphabet
      }
      if(is.list(x)){
        self$n <- sapply(x, length)
        self$data <- lapply(x, function(s) as.numeric(factor(s, levels = self$Alphabet$symbols)))
      } else if(is.character(x) | is.numeric(x)){
        self$n <- length(x)
        self$data <- list(as.numeric(factor(x, levels = self$Alphabet$symbols)))
      } else {
        stop("'x' must be either a list, a character vector or a numeric vector.")
      }
    },
    print = function(...) {
      cat("Alphabet:", self$Alphabet$symbols, "\n")
      n_seqs <- length(self$data)
      N <- sum(self$n)
      cat(glue("{n_seqs} Sequence(s) (Total observations = {N}):"), "\n")
      for(seq_index in seq_len(min(2, n_seqs))){
        cat(glue("Sequence {seq_index} (n = {self$n[seq_index]}):"))
        cat("\n")
      }
      if(n_seqs > 2){
        cat(glue("... and {n_seqs - 2} more sequence(s)."))
      }
    }
  ))
