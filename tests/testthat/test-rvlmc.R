test_that("vlmc generator works", {
  alph <- letters[1:3]
  tree <- ContextTree$new(alph, maximalDepth = 3, "root")
  tree$growActive("*")
  tree$growActive("*.c")
  context_list <- tree$getActiveNodes()

  rvlmc(n = 1000, alphabet = alph, context_list = context_list, context_probs = NULL)
})
