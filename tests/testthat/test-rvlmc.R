test_that("vlmc generator works", {
  alph <- letters[1:3]
  tree <- ContextTree$new(alph, maximalDepth = 3, "root")
  tree$growActive("*")
  tree$growActive("*.c")
  context_list <- tree$getActiveNodes()
  context_probs <- list(
    c(0.10, 0.20, 0.70), #.a
    c(0.33, 0.33, 0.34), #.b
    c(0.20, 0.10, 0.70), #.c.a
    c(0.01, 0.98, 0.01), #.c.b
    c(0.40, 0.40, 0.20)  #.c.c
  )
  n <- 1000

  s <- rvlmc(n = n, alphabet = alph,
             context_list = context_list, context_probs = context_probs)
  expect_length(s, 1000)
  expect_setequal(alph, unique(s))
})
