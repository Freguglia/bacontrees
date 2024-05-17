test_that("Alphabet class object creation works", {
  a <- bacontrees:::Alphabet$new(LETTERS[1:4])
  print(a)
})
