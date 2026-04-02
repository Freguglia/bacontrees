infer_alphabet <- function(data){
  data <- unlist(data)
  Alphabet$new(symbols = sort(unique(data)))
}

alphabet_equal <- function(alphabet1, alphabet2){
  length(setdiff(alphabet1$symbols, alphabet2$symbols)) == 0
}
