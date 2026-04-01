test_that("vlmc generator works", {
  alph <- letters[1:3]
  tree <- ContextTree$new(alphabet = alph, maximalDepth = 3, active = "root")
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

  n_vec <- c(1000, 999, 200)
  s_list <- rvlmc(n = n_vec, alphabet = alph,
                  context_list = context_list, context_probs = context_probs)
  expect_length(s_list, 3)
  expect_length(s_list[[1]], 1000)
  expect_length(s_list[[3]], 200)
})

test_that("rvlmc errors for invalid (incomplete) context_list", {
  # "*.a" and "*.b" alone do not cover all pasts if alphabet has 3 symbols
  expect_error(
    rvlmc(100, c("a", "b", "c"), c("*.a", "*.b"), list(c(0.5, 0.3, 0.2), c(0.5, 0.3, 0.2))),
    regexp = "incompatible with a full tree for the provided 'alphabet'"
  )
})

test_that("rvlmc errors when context_probs length differs from context_list", {
  alph <- c("a", "b")
  ctx  <- c("*.a", "*.b")
  expect_error(
    rvlmc(100, alph, ctx, list(c(0.5, 0.5))),
    regexp = "incompatible"
  )
})

test_that("rvlmc errors when probability vectors length differs from alphabet", {
  alph <- c("a", "b")
  ctx  <- c("*.a", "*.b")
  expect_error(
    rvlmc(100, alph, ctx, list(c(0.5, 0.3, 0.2), c(0.5, 0.3, 0.2))),
    regexp = "compatible"
  )
})

test_that("rvlmc errors when n has length 0", {
  alph <- c("a", "b")
  ctx  <- c("*.a", "*.b")
  probs <- list(c(0.5, 0.5), c(0.5, 0.5))
  expect_error(rvlmc(integer(0), alph, ctx, probs))
})
