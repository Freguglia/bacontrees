test_that("compress_logical on multiples of 4", {
  set.seed(1)
  n <- 12
  logical_vec <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
  compressed <- compress_logical(logical_vec)
  expect_type(compressed, "character")
  decompressed <- decompress_logical(compressed, n)
  expect_length(decompressed, 12)
  expect_identical(logical_vec, decompressed)
})

test_that("compress_logical on non-multiples of 4", {
  set.seed(1)
  n <- 11
  logical_vec <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
  compressed <- compress_logical(logical_vec)
  expect_type(compressed, "character")
  decompressed <- decompress_logical(compressed, n)
  expect_length(decompressed, 11)
  expect_identical(logical_vec, decompressed)
})
