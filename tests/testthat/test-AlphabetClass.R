test_that("Alphabet class object creation works", {
  a <- bacontrees:::Alphabet$new(LETTERS[1:4])
  expect_equal(a$length, 4)
  expect_identical(a$symbols, LETTERS[1:4])
  expect_identical(names(a$labels), as.character(1:4))

  b <- bacontrees:::Alphabet$new(LETTERS[1:20])
  expect_equal(b$length, 20)
  expect_identical(b$symbols, LETTERS[1:20])
  expect_identical(names(b$labels), as.character(1:20))

  out <- testthat::capture_output(b$print())
  expect_true(grepl("->", out))


})
