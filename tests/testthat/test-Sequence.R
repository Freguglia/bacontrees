test_that("Sequence initializes from a character vector", {
  s <- bacontrees:::Sequence$new(c("a", "b", "a", "c"))
  expect_equal(s$n, 4)
  expect_length(s$data, 1)
  expect_setequal(s$Alphabet$symbols, c("a", "b", "c"))
})

test_that("Sequence initializes from a list of vectors", {
  s <- bacontrees:::Sequence$new(list(c("a", "b"), c("b", "c", "a")))
  expect_equal(s$n, c(2, 3))
  expect_length(s$data, 2)
})

test_that("Sequence initializes from a numeric vector", {
  s <- bacontrees:::Sequence$new(c(1, 2, 1, 3))
  expect_equal(s$n, 4)
  expect_setequal(s$Alphabet$symbols, c("1", "2", "3"))
})

test_that("Sequence uses a manually provided alphabet", {
  alph <- bacontrees:::Alphabet$new(c("a", "b", "c"))
  s <- bacontrees:::Sequence$new(c("a", "b"), alphabet = alph)
  expect_identical(s$Alphabet$symbols, c("a", "b", "c"))
})

test_that("Sequence encodes data as integer indices into the alphabet", {
  s <- bacontrees:::Sequence$new(c("b", "a", "c"))
  alph <- s$Alphabet$symbols          # order matters for encoding
  expect_equal(s$data[[1]], as.numeric(factor(c("b", "a", "c"), levels = alph)))
})

test_that("Sequence print works without error", {
  s <- bacontrees:::Sequence$new(abc_list)
  expect_output(s$print(), "Sequence")
})

test_that("Sequence errors on unsupported input type", {
  expect_error(bacontrees:::Sequence$new(TRUE))
})
