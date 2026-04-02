Sequence <- R6Class(
  "Sequence",
  public = list(
    data = character(0),
    n = 0,
    Alphabet = NULL,
    initialize = function(x, alphabet = NULL) {
      if(is.null(alphabet)){
        self$Alphabet <- infer_alphabet(x)
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

#' @title Sequence Class
#' @name Sequence
#'
#' @description
#' The `Sequence` class provides a unified representation of one or more
#' sequences of categorical symbols. When initialized, the input is encoded as
#' integers according to a user-defined or automatically inferred alphabet.
#'
#' @details
#' The class accepts:
#'
#' - a **list** of character or numeric vectors (multiple sequences)
#' - a **single** character or numeric vector (one sequence)
#'
#' The alphabet can be provided manually or inferred via `infer_alphabet()`,
#' which must return an object containing a field `symbols` defining the
#' valid symbol set.
#'
#' Internally, all sequences are stored as integer vectors using
#' `as.numeric(factor(..., levels = Alphabet$symbols))`.
#'
#' @section Fields:
#' \describe{
#'   \item{\code{data}}{A list of integer-encoded sequences.}
#'   \item{\code{n}}{A vector with the length of each sequence.}
#'   \item{\code{Alphabet}}{An object defining the symbol set used for encoding.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(x, alphabet = NULL)}}{Create a new object from input.}
#'   \item{\code{print()}}{Display a compact summary.}
#' }
#'
#' @return
#' An R6 object of class `Sequence`.
#'
#' @examples
#' \dontrun{
#' seq <- Sequence$new(c("a", "b", "a", "c"))
#' seq$print()
#' }
#'
#' @export
Sequence
