chunk_size <- 4

compress_logical <- function(logical_vec) {
  chunks <- split(logical_vec, ceiling(seq_along(logical_vec) / chunk_size))

  chars <- sapply(chunks, function(chunk) {
    decimal <- sum(chunk * 2^(rev(seq_along(chunk) - 1)))
    intToUtf8(decimal + 65)
  })

  paste(chars, collapse = "")
}

decompress_logical <- function(compressed_string, n) {
  chars <- strsplit(compressed_string, "")[[1]]
  rem <- (n %% chunk_size)

  logical_vec <- unlist(lapply(chars, function(char) {
    decimal <- utf8ToInt(char) - 65
    binary <- as.integer(intToBits(decimal))
    rev(as.logical(binary[seq_len(chunk_size)]))
  }))

  len <- length(logical_vec)

  complete <- logical_vec
  if(rem == chunk_size){
    complete
  } else {
    complete[-((len-chunk_size+1):(len-rem))]
  }
}
