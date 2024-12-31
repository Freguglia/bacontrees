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
set.seed(1)
abc_list <- lapply(1:3, function(i) rvlmc(n = 1000, alph,
                                          context_list, context_probs))
abc_vec <- abc_list[[1]]

usethis::use_data(abc_vec, overwrite = TRUE)
usethis::use_data(abc_list, overwrite = TRUE)
